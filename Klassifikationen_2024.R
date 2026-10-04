# Klassifikationskarten 2024 erstellen in R
# install.packages(c('terra', 'sf', 'ggplot2', 'ggspatial', 'patchwork'))
# terra: Raster lesen und prüfen; sf: CRS beschreiben.
# ggplot2: Karten zeichnen; ggspatial: Maßstab/Nordpfeil.
# patchwork: mehrere Karten mit gemeinsamer Legende kombinieren.
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)

# Daten einlesen
data_dir <- 'C:/Users/longl/OneDrive - Universität Münster/Dokumente/GitHub/Bachelorthesis'
map_year <- 2024
training_year <- 2025
out_dir <- file.path(data_dir, 'Karten', as.character(map_year))
if (!dir.exists(data_dir)) stop('Der Datenordner existiert nicht: ', data_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# Vorhandene Dateien anzeigen.
list.files(data_dir, pattern = "\\.tif$", full.names = FALSE)

# Manuelle Darstellungsparameter
# 0.25: Maßstab soll ungefähr ein Viertel der Kartenbreite einnehmen.
# 0.9 cm: gut sichtbarer Nordpfeil in der Vergleichskarte; bei Bedarf verkleinern.
# 10 pt: Ausgangsschriftgröße; Lesbarkeit bei der endgültigen Druckgröße prüfen.
# Drei Legendenzeilen: lange Klassenbezeichnungen passen auch unter Einzelkarten.
# 30 x 24 cm: große Vergleichsabbildung; bei Verkleinerung in Word erneut prüfen.
# 300 dpi: gewählte Druckauflösung, kein Qualitätsgewinn für die Satellitendaten.
scale_width <- 0.25
arrow_size_cm <- 0.9
font_size <- 10
legend_rows <- 3
comparison_width_cm <- 30
comparison_height_cm <- 24
export_dpi <- 300
# Gleicher fester RGB-Stretch wie in GEE. Nicht für die Klassifikation verwendet.
rgb_min <- 0.02
rgb_max <- 0.35
if (rgb_min >= rgb_max) stop('rgb_min muss kleiner als rgb_max sein.')

# Klassifikationen einlesen
models <- c('RF', 'RF_GLCM', 'SVM', 'SVM_GLCM')
titles <- c('Random Forest', 'Random Forest + GLCM', 'SVM', 'SVM + GLCM')
files <- file.path(data_dir, paste0('NewDelta_geometry2_', models, '_', map_year, '.tif'))
missing_files <- files[!file.exists(files)]
if (length(missing_files) > 0) {
  stop('Diese Klassifikationsdateien fehlen:\n', paste(missing_files, collapse = '\n'))
}
rasters <- lapply(files, rast)


# Eingaben kontrollieren
# compareGeom prüft das Raster; es projiziert oder resampelt keine Daten.
# Klassenwerte sind 0, 1 und 2. NoData=255 muss beim GeoTIFF-Lesen als NA erscheinen.
for (r in rasters) {
  if (nlyr(r) != 1) stop('Klassenraster muss genau ein Band enthalten.')
  if (!compareGeom(rasters[[1]], r, stopOnError = FALSE)) stop('Rastergeometrien unterscheiden sich.')
  v <- unique(values(r)); v <- v[!is.na(v)]
  if (!all(v %in% 0:2)) stop('Unerwartete Klassenwerte; NoData muss als NA erkannt werden.')
}

# Gemeinsames Kartenkoordinatensystem
# Referenz: RF-Raster. Das aus GEE exportierte CRS wird beibehalten.
# Ein projiziertes Raster wird erwartet; für die GEE-Sentinel-2-Exporte ist dies UTM.
ref <- rasters[[1]]
if (is.lonlat(ref)) stop('Ein metrisches projiziertes Raster wird erwartet.')
map_crs <- st_crs(crs(ref))
if (is.na(map_crs)) stop('Das Raster hat kein gültiges Koordinatenreferenzsystem.')
# Projektion allein bedeutet nicht automatisch Meter: Einheit zusätzlich prüfen.
if (is.null(map_crs$units_gdal) ||
    !tolower(map_crs$units_gdal) %in% c('metre', 'meter', 'metres', 'meters', 'm')) {
  stop('Ein projiziertes Koordinatenreferenzsystem mit der Einheit Meter wird erwartet.')
}
pixel_size <- res(ref)
bbox <- as.vector(ext(ref))



# Einheitliche Klassenlegende
# Gemeinsame Ausdehnung und Koordinatenreferenz
# xlim/ylim verwenden die Einheiten von default_crs (hier UTM-Meter).
# crs und default_crs sind identisch: Pixelkoordinaten werden im ursprünglichen CRS dargestellt.
# expand=FALSE verhindert einen zusätzlichen Rand außerhalb der Rasterausdehnung.
# Quelle: offizielle ggplot2-Dokumentation zu coord_sf
base_map <- function() {
  coord_sf(crs = map_crs, default_crs = map_crs,
           xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE)
}

# Maßstab, Nordpfeil und Layout
# Der Maßstab wird durch ggspatial aus dem Karten-CRS und der Ausdehnung bestimmt.
# width_hint=0.25 ist eine gewünschte relative Länge, keine fest vorgegebene Distanz.
# which_north='true' orientiert den Pfeil geografisch nach Norden;
# Quelle: ggspatial-Dokumentation zu annotation_scale und annotation_north_arrow.
decorate <- function(p) {
  p + base_map() +
    annotation_scale(location = 'bl', width_hint = scale_width, unit_category = 'metric') +
    annotation_north_arrow(location = 'tr', which_north = 'true',
                           style = north_arrow_fancy_orienteering,
                           height = grid::unit(arrow_size_cm, 'cm'), width = grid::unit(arrow_size_cm, 'cm')) +
    theme_bw(base_size = font_size) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(face = 'bold'), legend.position = 'bottom',
          plot.caption = element_text(size = 9, hjust = 0),
          plot.margin = margin(8, 8, 8, 8))
}

# Karten pro Modell erstellen
# as.data.frame(..., xy=TRUE) liefert Pixelmittelpunkte und Klassenwerte.
# na.rm=FALSE erhält auch leere Zellen: NoData erscheint weiß.
# Faktor mit festen Levels: identische Farben und vollständige Legende in jeder Karte.
# geom_tile zeichnet die Pixel mit ihrer tatsächlichen Breite und Höhe.
# geom_tile ist mit coord_sf kompatibel und vermeidet die Meldung zu geom_raster.
# Breite und Höhe stammen aus dem GeoTIFF; es erfolgt kein Resampling.
plots <- lapply(seq_along(models), function(i) {
  d <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = FALSE)
  names(d)[3] <- 'class'
  d$class <- factor(d$class, levels = 0:2, labels = labels)
  subtitle <- sprintf('Training: %s | Anwendung: %s', training_year, map_year)
  decorate(ggplot(d, aes(x, y, fill = class)) +
             geom_tile(width = pixel_size[1], height = pixel_size[2], show.legend = TRUE) +
             scale_fill_manual(values = colours, limits = labels,
                               drop = FALSE, na.value = 'white', name = NULL) +
             guides(fill = guide_legend(nrow = legend_rows, byrow = TRUE)) +
             labs(title = titles[i], subtitle = subtitle))
})

# Gemeinsame Abbildung
# 2 × 2 Anordnung; guides='collect' führt die identischen Legenden zusammen.
# Quelle: offizielle patchwork-Dokumentation zu wrap_plots.
# Bildunterschrift: Datenquelle und tatsächliche Pixelgröße.
caption_lines <- c(
  sprintf(
    "Sentinel-2 SR Harmonized; eigene GEE-Auswertung. Raster: %g × %g m.",
    pixel_size[1], pixel_size[2]
  ),
  sprintf("Modelle mit Referenzdaten von %s trainiert; auf %s angewendet.",
          training_year, map_year),
  "geometry2: Kartenausschnitt. Keine eigenständige Genauigkeitsbewertung für 2024."
)

# Die einzelnen Texte durch Zeilenumbrüche verbinden.
caption_text <- paste(caption_lines, collapse = "\n")
# Legende auf mehrere Zeilen verteilen.
comparison <- wrap_plots(plots, ncol = 2, guides = 'collect') +
  plot_annotation(
    title = paste('New Delta: Klassifikationsvergleich', map_year),
    caption = caption_text,
    theme = theme(plot.caption = element_text(size = 9, hjust = 0),
                  plot.margin = margin(8, 8, 8, 8))
  ) & theme(legend.position = 'bottom')

# Vergleich und Einzelkarten speichern
ggsave(file.path(out_dir, paste0('Klassifikationsvergleich_', map_year, '.png')), comparison,
       width = comparison_width_cm, height = comparison_height_cm,
       units = 'cm', dpi = export_dpi, bg = 'white')
ggsave(file.path(out_dir, paste0('Klassifikationsvergleich_', map_year, '.pdf')), comparison,
       width = comparison_width_cm, height = comparison_height_cm,
       units = 'cm', bg = 'white')
for (i in seq_along(models)) {
  einzelkarte <- plots[[i]] +
    labs(caption = caption_text)
  ggsave(file.path(out_dir, paste0(models[i], '_geometry2_', map_year, '.png')),
         einzelkarte, width = 22, height = 16, units = 'cm',
         dpi = export_dpi, bg = 'white')
}

# RGB-Referenz separat, gleiche Ausdehnung, Nordpfeil und Massstab.
# RGB-Referenz für denselben Ausschnitt
rgb_file <- file.path(data_dir, paste0('NewDelta_geometry2_RGB_reflectance_', map_year, '.tif'))
if (file.exists(rgb_file)) {
  rgb <- rast(rgb_file)
  if (nlyr(rgb) != 3 || !compareGeom(ref, rgb[[1]], stopOnError = FALSE)) {
    stop('RGB-Raster hat unerwartete Bandzahl oder Geometrie.')
  }
  d <- as.data.frame(rgb, xy = TRUE, na.rm = FALSE)
  names(d) <- c('x','y','red','green','blue')
  stretch <- function(x) pmax(0, pmin(1, (x - rgb_min) / (rgb_max - rgb_min)))
  ok <- complete.cases(d[, 3:5])
  d$colour <- '#FFFFFF'
  d$colour[ok] <- grDevices::rgb(stretch(d$red[ok]), stretch(d$green[ok]), stretch(d$blue[ok]))
  p <- decorate(ggplot(d, aes(x, y, fill = colour)) + geom_tile(width = pixel_size[1], height = pixel_size[2]) +
                  scale_fill_identity() + labs(title = paste('Sentinel-2 RGB-Jahresmedian', map_year),
                                               caption = sprintf('B4 / B3 / B2; fester Reflexionsbereich: %.2f–%.2f.', rgb_min, rgb_max)))
  ggsave(file.path(out_dir, paste0('RGB_Referenz_', map_year, '.png')), p,
         width = 16, height = 16, units = 'cm', dpi = export_dpi, bg = 'white')
} else {
  message('RGB-Referenz übersprungen; Datei fehlt: ', rgb_file)
}

# Ergebnis anzeigen und verwendete Softwareversionen dokumentieren
capture.output(sessionInfo(),
               file = file.path(out_dir, paste0('Softwareversionen_', map_year, '.txt')))
print(comparison)
message('Karten gespeichert in: ', out_dir)


