#install.packages(c('terra', 'sf', 'ggplot2', 'ggspatial', 'patchwork'))
library(terra)
library(sf)
library(ggplot2)
library(ggspatial)
library(patchwork)

data_dir <- 'C:/Users/longl/OneDrive - Universität Münster/Dokumente/GitHub/Bachelorthesis'
out_dir <- file.path(data_dir, 'Karten')

# Vorhandene Dateien anzeigen.
list.files(data_dir, pattern = "\\.tif$", full.names = FALSE)

# Klassifikationen einlesen.
rf <- rast(file.path(
  data_dir, "NewDelta_geometry2_RF_2025.tif"
))

rf_glcm <- rast(file.path(
  data_dir, "NewDelta_geometry2_RF_GLCM_2025.tif"
))

svm <- rast(file.path(
  data_dir, "NewDelta_geometry2_SVM_2025.tif"
))

svm_glcm <- rast(file.path(
  data_dir, "NewDelta_geometry2_SVM_GLCM_2025.tif"
))

# RGB-Jahresmedian einlesen.
rgb_2025 <- rast(file.path(
  data_dir, "NewDelta_geometry2_RGB_reflectance_2025.tif"))


dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
models <- c('RF', 'RF_GLCM', 'SVM', 'SVM_GLCM')
titles <- c('Random Forest', 'Random Forest + GLCM', 'SVM', 'SVM + GLCM')
files <- file.path(data_dir, paste0('NewDelta_geometry2_', models, '_2025.tif'))
if (!all(file.exists(files))) stop('Die vier GeoTIFFs fehlen oder data_dir ist falsch.')
rasters <- lapply(files, rast)
for (r in rasters) {
  if (nlyr(r) != 1) stop('Klassenraster muss genau ein Band enthalten.')
  if (!compareGeom(rasters[[1]], r, stopOnError = FALSE)) stop('Rastergeometrien unterscheiden sich.')
  v <- unique(values(r)); v <- v[!is.na(v)]
  if (!all(v %in% 0:2)) stop('Unerwartete Klassenwerte; NoData muss als NA erkannt werden.')
}
ref <- rasters[[1]]
pixel_size <- terra::res(ref)
if (is.lonlat(ref)) stop('Ein metrisches projiziertes Raster wird erwartet.')
map_crs <- st_crs(crs(ref))
bbox <- as.vector(ext(ref))
labels <- c('Wüste / unbewachsener Boden', 'Pivotfelder', 'Andere Landwirtschaft')
colours <- setNames(c('#c2a878', '#7fff00', '#228B22'), labels)
summary_file <- file.path(data_dir, 'NewDelta_geometry2_ModelSummary.csv')
metrics <- if (file.exists(summary_file)) read.csv(summary_file) else NULL

base_map <- function() {
  coord_sf(crs = map_crs, default_crs = map_crs,
           xlim = bbox[1:2], ylim = bbox[3:4], expand = FALSE)
}
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
# Legende auf mehrere Zeilen verteilen.
comparison <- comparison &
  guides(fill = guide_legend(nrow = 3, byrow = TRUE)) &
  theme(
    legend.position = "bottom",
    legend.text = element_text(size = 10),
    plot.caption = element_text(size = 9, hjust = 0),
    plot.margin = margin(8, 8, 8, 8)
  )

# Vergleichskarte größer speichern.
ggsave(
  file.path(out_dir, "Klassifikationsvergleich_2025.png"),
  comparison,
  width = 30, height = 24, units = "cm",
  dpi = 300, bg = "white"
)

# Einzelkarten mit denselben Farben, Maßstäben und Nordpfeilen.
for (i in seq_along(models)) {
  einzelkarte <- plots[[i]] +
    guides(fill = guide_legend(nrow = 3, byrow = TRUE))
  
  ggsave(
    file.path(out_dir, paste0(models[i], "_geometry2_2025.png")),
    einzelkarte,
    width = 22, height = 16, units = "cm",
    dpi = 300, bg = "white"
  )
}



# RGB-Referenz separat, gleiche Ausdehnung, Nordpfeil und Massstab.
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
print(comparison)
message('Karten gespeichert in: ', out_dir)