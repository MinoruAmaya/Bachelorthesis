library(terra)

folder <- "C:/Users/longl/OneDrive - Universität Münster/Dokumente/GitHub/Bachelorthesis/ChangeDetection_2018_2025"
files <- list.files(
  folder,
  pattern = "^NewDelta_Change_[0-9]{4}_[0-9]{4}\\.tif$",
  full.names = TRUE
)

print(files)

# Ergebnistabelle vorbereiten
results <- data.frame(
  from_year = integer(),
  to_year = integer(),
  new_activity_ha = numeric(),
  activity_loss_ha = numeric(),
  net_change_ha = numeric()
)
  
# jeden Jahresübergang berechnen
for (f in files) {
  
  # Raster laden
  change <- rast(f)
  
  
  # Prüfen, ob die richtigen Bänder vorhanden sind
  if (!all(c(
    "new_activity",
    "activity_loss"
  ) %in% names(change))) {
    
    stop(
      paste(
        "Benötigte Bänder fehlen in:",
        basename(f)
      )
    )
  }
  
  # Pixelfläche in ha
  area_ha <- cellSize(
    change[[1]],
    unit = "ha"
  )
  
  # Fläche potenziell neuer Aktivität
  new_activity_ha <- global(
    
    area_ha *
      (change[["new_activity"]] == 1),
    
    fun = "sum",
    
    na.rm = TRUE
    
  )[1, 1]
  
  # Fläche potenzieller Aktivitätsverluste
  activity_loss_ha <- global(
    
    area_ha *
      (change[["activity_loss"]] == 1),
    
    fun = "sum",
    
    na.rm = TRUE
    
  )[1, 1]
  
  # Nettoflächenänderung
  net_change_ha <-
    new_activity_ha -
    activity_loss_ha
  
  # Jahre aus Dateinamen auslesen
  filename <- basename(f)
  
  year_match <- regmatches(
    filename,
    regexpr(
      "[0-9]{4}_[0-9]{4}",
      filename
    )
  )
  
  year_parts <- strsplit(
    year_match,
    "_"
  )[[1]]
  
  
  from_year <- as.integer(
    year_parts[1]
  )
  
  to_year <- as.integer(
    year_parts[2]
  )
  
  
  # Ergebnis speichern
  results <- rbind(
    
    results,
    
    data.frame(
      
      from_year = from_year,
      
      to_year = to_year,
      
      new_activity_ha =
        new_activity_ha,
      
      activity_loss_ha =
        activity_loss_ha,
      
      net_change_ha =
        net_change_ha
      
    )
  )
}

# nach Jahren sortieren
results <- results[
  order(results$from_year),
]


# Zeilennamen entfernen
rownames(results) <- NULL



# Ergebnisse
print(results)



# Gesamtsummen berechnen
total_new <- sum(results$new_activity_ha, na.rm = TRUE)
total_loss <- sum(results$activity_loss_ha, na.rm = TRUE)
total_net <- sum(results$net_change_ha, na.rm = TRUE)

# Gesamtzeile erstellen
gesamt <- data.frame(
  from_year = 2018,
  to_year = 2025,
  new_activity_ha = total_new,
  activity_loss_ha = total_loss,
  net_change_ha = total_net
)

# An Tabelle anhängen
results_mit_gesamt <- rbind(
  results,
  gesamt
)

print(results_mit_gesamt)