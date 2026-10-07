# install.packages(c("terra", "sf", "ggplot2", "ggspatial"))
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)

# Ordner und Darstellungsparameter.
data_dir <- paste0(
  "C:/Users/longl/OneDrive - Universität Münster/",
  "Dokumente/GitHub/Bachelorthesis/",
  "ChangeDetection_Postklassifikation"
)
change_dir <- file.path(data_dir, "ChangeDetection_R_geometry2")
map_dir <- file.path(
  data_dir, "Karten_Postklassifikation",
  "RF_ChangeDetection_geometry2"
)
dir.create(map_dir, recursive = TRUE, showWarnings = FALSE)

years <- 2018:2025

# Nur für die Vorschau: ungefähr maximal 600.000 Zellen.
preview_cells <- 600000L



# PNG Vorschauen für alles Jahresübergänge
change_labels <- c(
  "Unbewachsener Boden in beiden Jahren",
  "Landwirtschaft in beiden Jahren",
  "Mögliche Zunahme der Landwirtschaft",
  "Möglicher Rückgang der Landwirtschaft"
)

change_colours <- setNames(
  c("#E8DDC8", "#B8C9B3", "#00A650", "#D95F02"),
  change_labels
)

# Prüfen, ob alle benötigten Eingangskarten vorhanden sind.
files <- file.path(
  data_dir,
  paste0(
    "NewDelta_geometry2_RF_Classification_",
    years, ".tif"
  )
)

if (!all(file.exists(files))) {
  stop(
    "Diese Jahreskarten fehlen:\n",
    paste(basename(files[!file.exists(files)]), collapse = "\n")
  )
}

# TRUE: vorhandene Vorschau-PNGs bewusst ersetzen.
overwrite_png <- FALSE

for (i in seq_len(length(years) - 1L)) {
  
  from_year <- years[i]
  to_year   <- years[i + 1L]
  
  png_file <- file.path(
    map_dir,
    paste0(
      "NewDelta_RF_Postklassifikation_VORSCHAU_",
      from_year, "_", to_year, ".png"
    )
  )
  
  # Bereits fertiggestellte Karten nicht erneut berechnen.
  if (file.exists(png_file) && !overwrite_png) {
    message("Bereits vorhanden: ", basename(png_file))
    next
  }
  
  start_time <- Sys.time()
  message("Beginne: ", from_year, " → ", to_year)
  
  
  
  # Jahreskarten öffnen und Rastergitter prüfen
  before <- terra::rast(files[i])
  after  <- terra::rast(files[i + 1L])
  
  if (nlyr(before) != 1L || nlyr(after) != 1L) {
    stop("Jahreskarten müssen jeweils ein Band enthalten.")
  }
  
  terra::compareGeom(before, after, stopOnError = TRUE)
  
  map_crs <- sf::st_crs(terra::crs(before))
  crs_unit <- map_crs$units_gdal
  
  if (is.na(map_crs) || terra::is.lonlat(before) ||
      is.null(crs_unit) || is.na(crs_unit) ||
      !tolower(crs_unit) %in%
      c("metre", "meter", "metres", "meters", "m")) {
    stop("Ein projiziertes Raster in Metern wird erwartet.")
  }
  
  bbox <- as.vector(terra::ext(before))
  
  
  
  # Beide Jahre gemeinsam für die Vorschau abtasten
  
  annual_pair <- c(before, after)
  
  shown <- if (terra::ncell(before) > preview_cells) {
    terra::spatSample(
      annual_pair,
      size = preview_cells,
      method = "regular",
      as.raster = TRUE
    )
  } else {
    annual_pair
  }
  
  d <- as.data.frame(shown, xy = TRUE, na.rm = FALSE)
  names(d) <- c("x", "y", "before", "after")
  
  d$before[d$before == 255] <- NA
  d$after[d$after == 255]   <- NA
  
  valid <- complete.cases(d[, c("before", "after")])
  
  if (!any(valid)) {
    stop("Keine gemeinsam gültigen Pixel: ", from_year, "–", to_year)
  }
  
  if (any(!d$before[valid] %in% 0:2) ||
      any(!d$after[valid] %in% 0:2)) {
    stop("Unerwartete Klassenwerte: ", from_year, "–", to_year)
  }
  
  
  
  
  # Klassenübergänge bestimmen
  
  # Klassen 1 und 2 zählen gemeinsam als Landwirtschaft.
  # Fehlende Werte bleiben NA.
  
  agri_before <- d$before > 0
  agri_after  <- d$after > 0
  
  pair_code <- 2L * agri_before + agri_after
  
  # Umkodierung in die Reihenfolge der Legende.
  lookup <- c(0L, 2L, 3L, 1L)
  d$code <- lookup[pair_code + 1L]
  
  d$category <- factor(
    d$code,
    levels = 0:3,
    labels = change_labels
  )
  
  pixel_size <- terra::res(shown)
  
  
  # Fertige Karte zusammenstellen
  p <- ggplot(d, aes(x = x, y = y, fill = category)) +
    geom_raster(interpolate = FALSE) +
    scale_fill_manual(
      values = change_colours,
      limits = change_labels,
      drop = FALSE,
      na.value = "white",
      name = NULL
    ) +
    coord_fixed(
      ratio = 1,
      xlim = bbox[1:2],
      ylim = bbox[3:4],
      expand = FALSE
    ) +
    annotation_scale(
      location = "bl",
      plot_unit = "m",
      unit_category = "metric",
      width_hint = 0.25
    ) +
    annotation_north_arrow(
      location = "tr",
      which_north = "grid",
      style = north_arrow_fancy_orienteering,
      height = grid::unit(0.9, "cm"),
      width = grid::unit(0.9, "cm")
    ) +
    guides(fill = guide_legend(ncol = 1)) +
    labs(
      title = paste0(
        "New Delta: Landwirtschaftsveränderung ",
        from_year, "–", to_year
      ),
      subtitle = "Postklassifikationsvergleich der jährlichen RF-Karten",
      caption = paste0(
        "RF mit Training 2025; Ausschnitt geometry2.\n",
        "Vorschau: regelmäßige Abtastung, Darstellungsraster ",
        paste(round(pixel_size, 1), collapse = " × "), " m.\n",
        "Klassenwechsel können Klassifikationsfehler enthalten. ",
        "Weiß: fehlende Daten. Nordpfeil: Gitternord."
      )
    ) +
    theme_bw(base_size = 11) +
    theme(
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 9),
      plot.title = element_text(face = "bold"),
      plot.caption = element_text(hjust = 0, size = 8)
    )
  
  
  # PNG sofort speichern
  ggsave(
    filename = png_file,
    plot = p,
    width = 24,
    height = 22,
    units = "cm",
    dpi = 200,
    bg = "white"
  )
  
  message(
    "Gespeichert: ", basename(png_file),
    " | Dauer: ",
    round(
      as.numeric(difftime(Sys.time(), start_time, units = "secs")),
      1
    ),
    " Sekunden."
  )
  
  # Speicher für den nächsten Jahresübergang freigeben.
  rm(before, after, annual_pair, shown, d, p,
     agri_before, agri_after, pair_code, valid)
  invisible(gc())
}

message("Fertig. PNG-Karten liegen in: ", map_dir)

# Ergebnisordner im Windows-Explorer öffnen.
shell.exec(normalizePath(map_dir, winslash = "\\"))