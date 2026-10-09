# Klassifikationskarten erstellen in R
# Trainings, Klassifikation, Validierung in GEE


# install.packages(c('terra', 'sf', 'ggplot2', 'ggspatial', 'patchwork'))
# terra: Raster lesen und prüfen; sf: CRS beschreiben.
# ggplot2: Karten zeichnen; ggspatial: Maßstab/Nordpfeil.
# patchwork: mehrere Karten mit gemeinsamer Legende kombinieren.
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)
library(ranger)

# Daten einlesen
data_dir <- 'C:/Users/longl/OneDrive - Universität Münster/Dokumente/GitHub/Bachelorthesis/Klassifikationen_GEE_Exporte'
out_dir <- file.path(data_dir, 'Karten')
if (!dir.exists(data_dir)) stop('Der Datenordner existiert nicht: ', data_dir)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

base_bands <- c(
  "B2_median", "B3_median", "B4_median", "B5_median",
  "B8_median", "B8A_median", "B11_median", "B12_median",
  "NDVI_median", "NDVI_max", "NDVI_stdDev",
  "NDMI_median", "NDRE_median", "BSI_median"
)

glcm_bands <- c(
  base_bands,
  "NDVI_texture_contrast",
  "NDVI_texture_entropy",
  "NDVI_texture_variance"
)

read_samples <- function(filename, bands) {
  
  path <- file.path(data_dir, filename)
  
  if (!file.exists(path)) {
    stop("Datei fehlt: ", path)
  }
  
  d <- read.csv(path, check.names = FALSE)
  
  required <- c(bands, "class")
  
  if (!all(required %in% names(d))) {
    stop("Erforderliche Spalten fehlen in: ", filename)
  }
  
  # Nur Prädiktoren und Referenzklasse übernehmen.
  # Andere Exportspalten fließen nicht in das Training ein.
  d <- d[, required, drop = FALSE]
  
  if (!all(vapply(d[bands], is.numeric, logical(1)))) {
    stop("Prädiktoren müssen numerisch sein: ", filename)
  }
  
  if (any(!is.finite(as.matrix(d[bands])))) {
    stop("Fehlende oder ungültige Prädiktorwerte: ", filename)
  }
  
  if (anyNA(d$class) || !all(d$class %in% 0:2)) {
    stop("Ungültige Klassenwerte: ", filename)
  }
  
  # Faktor erzwingt Klassifikation statt Regression.
  d$class <- factor(d$class, levels = 0:2)
  
  if (any(table(d$class) == 0)) {
    stop("Mindestens eine Klasse fehlt: ", filename)
  }
  
  d
}



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
files <- file.path(data_dir, paste0('NewDelta_geometry2_', models, '_2025.tif'))
if (!all(file.exists(files))) stop('Die vier GeoTIFFs fehlen oder data_dir ist falsch.')
rasters <- lapply(files, rast)



# Eingaben kontrollieren
# Alle Karten sollen identische Ausdehnung, CRS, Zellzahl und Auflösung besitzen.
# compareGeom prüft das Raster; es projiziert oder resampelt keine Daten.
# Klassenwerte sind 0, 1 und 2. NoData=255 muss beim GeoTIFF-Lesen als NA erscheinen.
# Die vollständigen Zellwerte werden für diese Kontrolle in den Arbeitsspeicher geladen.
for (r in rasters) {
  if (nlyr(r) != 1) stop('Klassenraster muss genau ein Band enthalten.')
  if (!compareGeom(rasters[[1]], r, stopOnError = FALSE)) stop('Rastergeometrien unterscheiden sich.')
  v <- unique(values(r)); v <- v[!is.na(v)]
  if (!all(v %in% 0:2)) stop('Unerwartete Klassenwerte; NoData muss als NA erkannt werden.')
}


# Gemeinsames Kartenkoordinaten
# Referenz: RF-Raster.aus GEE exportierte CRS wird verwenden.
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
# Braun: Wüste/Boden; Hellgrün: Pivotfelder; Dunkelgrün: andere Landwirtschaft.
# Das sind Darstellungsentscheidungen; keine zusätzliche Analyse oder Klassentrennung.
labels <- c('Wüste / unbewachsener Boden', 'Pivotfelder', 'Andere Landwirtschaft')
colours <- setNames(c('#c2a878', '#7fff00', '#228B22'), labels)



# Genauigkeiten aus GEE einlesen
summary_file <- file.path(data_dir, 'NewDelta_geometry2_ModelSummary.csv')
metrics <- if (file.exists(summary_file)) read.csv(summary_file) else NULL
if (!is.null(metrics)) {
  required_columns <- c('model', 'overall_accuracy', 'kappa')
  if (!all(required_columns %in% names(metrics))) {
    stop('Genauigkeitstabelle benötigt model, overall_accuracy und kappa.')
  }
  if (anyDuplicated(metrics$model)) stop('Doppelte Modelle in der Genauigkeitstabelle.')
  if (!is.numeric(metrics$overall_accuracy) || !is.numeric(metrics$kappa)) {
    stop('OA und Kappa müssen numerische Spalten sein.')
  }
  if (any(!is.finite(metrics$overall_accuracy)) ||
      any(metrics$overall_accuracy < 0 | metrics$overall_accuracy > 1) ||
      any(!is.finite(metrics$kappa)) || any(abs(metrics$kappa) > 1)) {
    stop('Ungültige Genauigkeitswerte: OA als Anteil 0–1, Kappa zwischen -1 und 1 erwartet.')
  }
}




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
    annotation_scale(location = 'bl', width_hint = 0.25, unit_category = 'metric') +
    annotation_north_arrow(location = 'tr', which_north = 'true',
                           style = north_arrow_fancy_orienteering,
                           height = grid::unit(0.9, 'cm'), width = grid::unit(0.9, 'cm')) +
    theme_bw(base_size = 10) +
    theme(axis.title = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), panel.grid = element_blank(),
          plot.title = element_text(face = 'bold'), legend.position = 'bottom')
}



# Karten pro Modell erstellen
# as.data.frame(..., xy=TRUE) liefert Pixelmittelpunkte und Klassenwerte.
# na.rm=FALSE erhält auch leere Zellen: NoData erscheint weiß.
# Faktor mit festen Levels: identische Farben und vollständige Legende in jeder Karte.
# geom_tile zeichnet die Pixel mit ihrer tatsächlichen Breite und Höhe.
# OA wird nur für die Beschriftung mit 100 multipliziert; Kappa bleibt unverändert.
plots <- lapply(seq_along(models), function(i) {
  d <- as.data.frame(rasters[[i]], xy = TRUE, na.rm = FALSE)
  names(d)[3] <- 'class'
  d$class <- factor(d$class, levels = 0:2, labels = labels)
  subtitle <- NULL
  if (!is.null(metrics)) {
    row <- metrics[metrics$model == models[i], ]
    if (nrow(row) == 1) subtitle <- sprintf('OA: %.2f %% | Kappa: %.3f',
                                            row$overall_accuracy * 100, row$kappa)
  }
  decorate(ggplot(d, aes(x, y, fill = class)) +
             geom_raster() + scale_fill_manual(values = colours, limits = labels,
                                               drop = FALSE, na.value = 'white', name = NULL) +
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
  "geometry2: Kartenausschnitt; keine separate Validierung."
)

# Hinweis zur Genauigkeit nur ergänzen, wenn die CSV eingelesen wurde.
if (!is.null(metrics)) {
  caption_lines <- c(
    caption_lines,
    "OA/Kappa: jeweilige Validierungsstichprobe im gesamten Untersuchungsgebiet."
  )
}

# Die einzelnen Texte durch Zeilenumbrüche verbinden.
caption_text <- paste(caption_lines, collapse = "\n")
# Legende auf mehrere Zeilen verteilen.
comparison <- wrap_plots(plots, ncol = 2, guides = 'collect') +
  plot_annotation(
    title = 'New Delta: Klassifikationsvergleich 2025',
    caption = caption_text,
    theme = theme(plot.caption = element_text(size = 9, hjust = 0),
                  plot.margin = margin(8, 8, 8, 8))
  ) & theme(legend.position = 'bottom')

# Vergleichskarte größer speichern.
ggsave(
  file.path(out_dir, "Klassifikationsvergleich_2025.png"),
  comparison,
  width = 30, height = 24, units = "cm",
  dpi = 300, bg = "white"
)




# Vergleich und Einzelkarten speichern
ggsave(file.path(out_dir, 'Klassifikationsvergleich_2025.png'), comparison,
       width = comparison_width_cm, height = comparison_height_cm,
       units = 'cm', dpi = export_dpi, bg = 'white')
ggsave(file.path(out_dir, 'Klassifikationsvergleich_2025.pdf'), comparison,
       width = comparison_width_cm, height = comparison_height_cm,
       units = 'cm', bg = 'white')
for (i in seq_along(models)) {
  einzelkarte <- plots[[i]] +
    labs(caption = 'Sentinel-2 SR Harmonized; eigene GEE-Auswertung. Ausschnitt: geometry2.')
  ggsave(file.path(out_dir, paste0(models[i], '_geometry2_2025.png')),
         einzelkarte, width = 22, height = 16, units = 'cm',
         dpi = export_dpi, bg = 'white')
}



# RGB-Referenz separat, gleiche Ausdehnung, Nordpfeil und Massstab.
# RGB-Referenz für denselben Ausschnitt
rgb_file <- file.path(data_dir, 'NewDelta_geometry2_RGB_reflectance_2025.tif')
if (file.exists(rgb_file)) {
  rgb <- rast(rgb_file)
  if (nlyr(rgb) != 3 || !compareGeom(ref, rgb[[1]], stopOnError = FALSE)) {
    stop('RGB-Raster hat unerwartete Bandzahl oder Geometrie.')
  }
  d <- as.data.frame(rgb, xy = TRUE, na.rm = FALSE)
  names(d) <- c('x','y','red','green','blue')
  stretch <- function(x) pmax(0, pmin(1, (x - 0.02) / (0.35 - 0.02)))
  ok <- complete.cases(d[, 3:5])
  d$colour <- '#FFFFFF'
  d$colour[ok] <- grDevices::rgb(stretch(d$red[ok]), stretch(d$green[ok]), stretch(d$blue[ok]))
  p <- decorate(ggplot(d, aes(x, y, fill = colour)) + geom_raster() +
                  scale_fill_identity() + labs(title = 'Sentinel-2 RGB-Jahresmedian 2025',
                                               caption = 'B4 / B3 / B2; feste Darstellung: Reflexion 0,02–0,35.'))
  ggsave(file.path(out_dir, 'RGB_Referenz_2025.png'), p,
         width = 16, height = 16, units = 'cm', dpi = 300, bg = 'white')
}


# Ergebnis anzeigen und Softwareversionen dokumentieren
print(comparison)
message('Karten gespeichert in: ', out_dir)




# Feature Importance an RF 
analyse_rf <- function(training_file, validation_file,
                       bands, model_name) {
  
  train <- read_samples(training_file, bands)
  validation <- read_samples(validation_file, bands)
  # Orientierung an bisherigen GEE-Einstellungen:
  # 200 Bäume; sqrt(p) Kandidaten je Aufteilung;
  # 70 % Stichprobenanteil je Baum.
  #
  # ranger und GEE/Smile sind unterschiedliche Implementierungen.
  # Gleiche Zahlen garantieren keine identischen Modelle.
  model <- ranger(
    dependent.variable.name = "class",
    data = train,
    num.trees = 200,
    mtry = floor(sqrt(length(bands))),
    min.node.size = 1,
    min.bucket = 2,
    replace = FALSE,
    sample.fraction = 0.7,
    splitrule = "gini",
    importance = "permutation",
    scale.permutation.importance = FALSE,
    seed = 42,
    num.threads = 2
  )
  
  # ranger berechnet diese Importance anhand der
  # jeweiligen Out-of-bag-Beobachtungen.
  importance <- data.frame(
    predictor = names(model$variable.importance),
    permutation_importance = unname(model$variable.importance)
  )
  
  importance <- importance[
    order(importance$permutation_importance, decreasing = TRUE),
    ,
    drop = FALSE
  ]
  
  # Bewertung auf den ursprünglichen Hold-out-Daten.
  prediction <- predict(
    model,
    data = validation[, bands, drop = FALSE]
  )$predictions
  
  cm <- table(
    Referenz = validation$class,
    Vorhersage = factor(prediction, levels = 0:2)
  )
  
  n <- sum(cm)
  oa <- sum(diag(cm)) / n
  
  # Erwartete Übereinstimmung aus den Randverteilungen.
  expected <- sum(rowSums(cm) * colSums(cm)) / n^2
  
  kappa <- if (expected < 1) {
    (oa - expected) / (1 - expected)
  } else {
    NA_real_
  }
  
  cat("\n", model_name, "\n", sep = "")
  cat("Trainingspixel je Klasse:\n")
  print(table(train$class))
  
  cat("Hold-out-Konfusionsmatrix:\n")
  print(cm)
  
  cat(sprintf("Hold-out-OA: %.2f %%\n", oa * 100))
  cat(sprintf("Hold-out-Kappa: %.3f\n", kappa))
  
  cat("Permutation Importance:\n")
  print(importance, row.names = FALSE)
  
  # Darstellung im RStudio-Plots-Fenster.
  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  
  par(mar = c(5, 12, 4, 2))
  
  barplot(
    rev(importance$permutation_importance),
    names.arg = rev(importance$predictor),
    horiz = TRUE,
    las = 1,
    cex.names = 0.75,
    col = "#228B22",
    main = paste(model_name, "– Permutation Importance"),
    xlab = "Zunahme des OOB-Vorhersagefehlers"
  )
  
  abline(v = 0, col = "grey40")
  
  list(
    model = model,
    importance = importance,
    confusion_matrix = cm,
    overall_accuracy = oa,
    kappa = kappa
  )
}

# Beide RF-Varianten untersuchen
rf_r <- analyse_rf(
  training_file = "NewDelta_RF_Training_2025.csv",
  validation_file = "NewDelta_RF_Validation_2025.csv",
  bands = base_bands,
  model_name = "RF in R"
)

rf_glcm_r <- analyse_rf(
  training_file = "NewDelta_RF_GLCM_Training_2025.csv",
  validation_file = "NewDelta_RF_GLCM_Validation_2025.csv",
  bands = glcm_bands,
  model_name = "RF + GLCM in R"
)


