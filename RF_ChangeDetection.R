# install.packages(c("terra", "sf", "ggplot2", "ggspatial"))
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)

# Ordner und Darstellungsparameter.
data_dir <- "C:/Users/longl/OneDrive - Universität Münster/Dokumente/GitHub/Bachelorthesis/ChangeDetection_Postklassifikation"
change_dir <- file.path(data_dir, "ChangeDetection_R_geometry2")
map_dir <- file.path(data_dir, "Karten", "RF_ChangeDetection_geometry2")
dir.create(change_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)
years <- 2018:2025
overwrite_outputs <- FALSE # TRUE für bewusstes Ersetzen alter Ergebnisse.
make_maps <- TRUE
display_factor <- 1L       # 1 = alle Pixel; >1: nur Kartenanzeige gröber.
# Anzeigevergröberung beeinflusst weder Analyse-GeoTIFFs noch Flächentabellen.
stopifnot(length(display_factor) == 1L, is.finite(display_factor),
          display_factor >= 1, display_factor %% 1 == 0)
terraOptions(memfrac = 0.5, progress = 1)
write_options <- list(datatype = "INT1U", NAflag = 255,
                      gdal = c("COMPRESS=LZW", "TILED=YES"))



# Jahreskarten lesen und prüfen
files <- file.path(data_dir, paste0("NewDelta_RF_Classification_", years, ".tif"))
missing <- files[!file.exists(files)]
if (length(missing)) stop("Zuerst fehlende Jahreskarten herunterladen:\n",
                          paste(basename(missing), collapse = "\n"))
maps <- setNames(lapply(files, terra::rast), years)
check_values <- function(r, allowed, label) {
  tab <- terra::freq(r, digits = NA)
  if (is.null(tab) || !nrow(tab)) stop(label, ": keine gültigen Pixel.")
  if (any(!tab$value %in% allowed)) stop(label, ": unerwartete Klassenwerte.")
}
for (i in seq_along(maps)) {
  r <- maps[[i]]
  if (terra::nlyr(r) != 1L) stop("Jahreskarte benötigt genau ein Band.")
  # Normalerweise erkennt terra NoData bereits. Zusätzliche Absicherung:
  r <- terra::classify(r, matrix(c(255, NA_real_), ncol = 2))
  check_values(r, 0:2, paste("Jahr", years[i]))
  maps[[i]] <- r
}
ref <- maps[[1]]
for (r in maps) {
  if (!terra::compareGeom(ref, r, stopOnError = FALSE))
    stop("Rastergitter unterscheiden sich. Gemeinsames GEE-Exportgitter prüfen.")
}
map_crs <- sf::st_crs(terra::crs(ref))
if (is.na(map_crs)) stop("Koordinatenreferenzsystem fehlt.")
crs_unit <- map_crs$units_gdal
if (terra::is.lonlat(ref) || is.null(crs_unit) || is.na(crs_unit) ||
    !tolower(crs_unit) %in% c("metre", "meter", "metres", "meters", "m"))
  stop("Ein projiziertes Raster mit Koordinaten in Metern wird erwartet.")
bbox <- as.vector(terra::ext(ref))


# Vollständige Übergänge und vereinfachte Landwirtschaftsänderung
# Code = 3 * Klasse im früheren Jahr + Klasse im späteren Jahr.
class_labels <- c("Wüste / unbewachsener Boden", "Pivotfelder", "Andere Landwirtschaft")
transition_table <- data.frame(code = 0:8, from_class = rep(0:2, each = 3),
                               to_class = rep(0:2, times = 3))
transition_table$from_label <- class_labels[transition_table$from_class + 1L]
transition_table$to_label <- class_labels[transition_table$to_class + 1L]
transition_table$label <- paste(transition_table$from_label,
                                transition_table$to_label, sep = " → ")
# Pivot <-> andere Landwirtschaft bleibt in der Zusammenfassung Landwirtschaft.
transition_table$agri_code <- c(0, 2, 2, 3, 1, 1, 3, 1, 1)
agri_labels <- c("Unbewachsener Boden in beiden Jahren",
                 "Landwirtschaft in beiden Jahren",
                 "Mögliche Zunahme der Landwirtschaft",
                 "Möglicher Rückgang der Landwirtschaft")
agri_colours <- c("#c2a878", "#228B22", "#00ff66", "#e6550d")
transition_colours <- c("#c2a878", "#66c2a4", "#00a65a", "#e6550d",
                        "#7fff00", "#8073ac", "#a63603", "#df65b0", "#228B22")
transition_table$agri_label <- agri_labels[transition_table$agri_code + 1L]
write.csv(transition_table, file.path(change_dir, "RF_Uebergangscodes.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
lookup <- as.matrix(transition_table[, c("code", "agri_code")])


# Karten gemeinsamer Ausschnitt, Maßstab und geografischer Nordpfeil
save_map <- function(r, labels, colours, title, filename) {
  shown <- if (display_factor > 1L) {
    terra::aggregate(r, fact = display_factor, fun = "modal", na.rm = TRUE)
  } else r
  d <- as.data.frame(shown, xy = TRUE, na.rm = TRUE)
  names(d)[3] <- "code"
  d$category <- factor(d$code, levels = seq_along(labels) - 1L, labels = labels)
  size <- terra::res(shown)
  caption <- paste0("RF mit Training 2025; jährliche Sentinel-2-Prädiktoren.\n",
                    "Postklassifikationsvergleich; Veränderungen können Klassifikationsfehler enthalten.\n",
                    "Weiß: keine gemeinsame gültige Beobachtung. Darstellung: ",
                    paste(format(size, trim = TRUE), collapse = " × "), " m.")
  p <- ggplot(d, aes(x = x, y = y, fill = category)) +
    # geom_tile vermeidet die geom_raster/coord_sf-Kompatibilitätsmeldung.
    geom_tile(width = size[1], height = size[2]) +
    scale_fill_manual(values = setNames(colours, labels), limits = labels,
                      drop = FALSE, name = NULL) +
    coord_sf(crs = map_crs, default_crs = map_crs,
             xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE) +
    annotation_scale(location = "bl", width_hint = 0.25, unit_category = "metric") +
    annotation_north_arrow(location = "tr", which_north = "true",
                           style = north_arrow_fancy_orienteering,
                           height = grid::unit(0.9, "cm"), width = grid::unit(0.9, "cm")) +
    guides(fill = guide_legend(ncol = 1)) +
    labs(title = title, caption = caption) + theme_bw(base_size = 10) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          legend.position = "bottom", legend.text = element_text(size = 9),
          plot.title = element_text(face = "bold"),
          plot.caption = element_text(hjust = 0, size = 8))
  for (extension in c("png", "pdf"))
    ggsave(file.path(map_dir, paste0(filename, ".", extension)), p,
           width = 26, height = 24, units = "cm", dpi = 300, bg = "white")
}




# lokale Verarbeitung
# Fläche in ha unter Berücksichtigung der Erdoberfläche, einmal pro Gitter.
# transform=TRUE betrifft nur die Flächenberechnung, nicht die Klassenkarten.
# rcx verhindert eine gröbere Zwischenberechnung der Zellflächen.
cell_ha <- terra::cellSize(ref, unit = "ha", mask = FALSE,
                           transform = TRUE, rcx = max(nrow(ref), ncol(ref)))
area_rows <- list()
agri_rows <- list()
for (i in seq_len(length(years) - 1L)) {
  a <- years[i]
  b <- years[i + 1L]
  pair <- paste(a, b, sep = "_")
  before <- maps[[as.character(a)]]
  after <- maps[[as.character(b)]]
  transition_file <- file.path(change_dir, paste0("NewDelta_RF_Transitions_", pair, ".tif"))
  change_file <- file.path(change_dir, paste0("NewDelta_RF_AgriChange_", pair, ".tif"))
  # NA in einem der Jahre bleibt im Ergebnis NA.
  # Beide Ausgabedateien werden separat geprüft, damit keine übersprungen wird.
  if (!file.exists(transition_file) || overwrite_outputs) {
    transition <- before * 3 + after
    names(transition) <- "transition"
    terra::writeRaster(transition, transition_file,
                       overwrite = overwrite_outputs, wopt = write_options)
  }
  transition <- terra::rast(transition_file)
  terra::compareGeom(ref, transition, stopOnError = TRUE)
  check_values(transition, 0:8, paste("Übergang", pair))
  if (!file.exists(change_file) || overwrite_outputs)
    terra::classify(transition, lookup, others = NA, filename = change_file,
                    overwrite = overwrite_outputs, wopt = write_options)
  change <- terra::rast(change_file)
  terra::compareGeom(ref, change, stopOnError = TRUE)
  check_values(change, 0:3, paste("AgriChange", pair))
  
  # Die Summe umfasst ausschließlich gemeinsam gültige Pixel.
  areas <- terra::zonal(cell_ha, transition, fun = "sum", na.rm = TRUE)
  totals <- rep(0, 9)
  totals[match(areas[[1]], 0:8)] <- areas[[2]]
  counts <- terra::freq(transition, digits = NA)
  pixels <- rep(0, 9)
  pixels[match(counts$value, 0:8)] <- counts$count
  row <- transition_table
  row$from_year <- a
  row$to_year <- b
  row$pixel_count <- pixels
  row$area_ha <- totals
  row$percent_common_valid_area <- 100 * totals / sum(totals)
  area_rows[[pair]] <- row
  agri_total <- vapply(0:3, function(k) sum(totals[row$agri_code == k]), numeric(1))
  agri_rows[[pair]] <- data.frame(from_year = a, to_year = b, agri_code = 0:3,
                                  label = agri_labels, area_ha = agri_total,
                                  percent_common_valid_area = 100 * agri_total / sum(totals))
  if (make_maps) {
    save_map(transition, transition_table$label, transition_colours,
             paste("RF: Klassenübergänge", a, "→", b), paste0("RF_Transitions_", pair))
    save_map(change, agri_labels, agri_colours,
             paste("RF: Landwirtschaftsveränderung", a, "→", b), paste0("RF_AgriChange_", pair))
  }
  message("Fertig: ", a, " → ", b)
}
write.csv(do.call(rbind, area_rows), file.path(change_dir, "RF_Transitions_Flaechen_ha.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
write.csv(do.call(rbind, agri_rows), file.path(change_dir, "RF_AgriChange_Flaechen_ha.csv"),
          row.names = FALSE, fileEncoding = "UTF-8")
writeLines(capture.output(sessionInfo()), file.path(change_dir, "R_SessionInfo.txt"))
message("GeoTIFFs und Tabellen: ", change_dir, "\nKarten: ", map_dir)
