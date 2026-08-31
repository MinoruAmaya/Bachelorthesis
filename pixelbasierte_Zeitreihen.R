# Einmalig installieren
install.packages("bfast")

#Pakete laden
library(tidyverse)
library(lubridate)
library(bfast)


#GEE CSV einlesen
time_series <- read.csv(
  "NewDelta_TimeSeries_2018_2025.csv"
)


#Datum vorbereiten
time_series$date <- as.Date(
  paste0(time_series$date, "-01")
)
#Daten nach Sample und Datum sortieren
time_series <- time_series %>%
  arrange(id, date)


#Grundlegende Kontrolle

# Anzahl aller Zeilen
nrow(time_series)

# Anzahl Beobachtungen pro Sample
table(time_series$id)


# Zuordnung ID <-> Typ
table(
  time_series$id,
  time_series$type
)

# Fehlende Werte kontrollieren
colSums(
  is.na(time_series)
)

# Zeitraum kontrollieren
range(time_series$date)


#NDVI Zeitreihe zunächst ohne BFAST ansehen:
ggplot(
  time_series,
  aes(
    x = date,
    y = NDVI
  )
) +
  geom_line() +
  facet_wrap(
    ~ id,
    ncol = 3
  ) +
  labs(
    title = "Monatliche NDVI-Zeitreihen 2018-2025",
    x = "Jahr",
    y = "NDVI"
  ) +
  theme_minimal()






#Funktion für BFAST Lite
run_bfast <- function(sample_data) {
  
  # Zeitlich sortieren
  sample_data <- sample_data %>%
    arrange(date)
  
  
  # NDVI-Werte als monatliche Zeitreihe definieren
  # frequency = 12, da 12 Monatswerte pro Jahr vorliegen
  
  ndvi_ts <- ts(
    sample_data$NDVI,
    start = c(2018, 1),
    frequency = 12
  )
  
  
  # BFAST Lite auf die NDVI-Zeitreihe anwenden
  model <- bfastlite(
    ndvi_ts
  )
  
  
  return(model)
}

#Daten auf Samples aufteilen
sample_list <- split(
  time_series,
  time_series$id
)


#BFAST Lite auf alle 9 Samples anwenden
bfast_results <- lapply(
  sample_list,
  run_bfast
)



# Namen der erzeugten Modelle kontrollieren
names(bfast_results)


#BFAST Ergebnisse aller 9 Samples darstellen
# 9 Diagramme auf einer Seite darstellen
par(
  mfrow = c(3, 3)
)


for (sample_name in names(bfast_results)) {
  
  plot(
    bfast_results[[sample_name]],
    breaks ="LWZ",
    main = sample_name,
    xlab = "Jahr",
    ylab = "NDVI"
  )
  
}


# Plot-Einstellung anschließend wieder zurücksetzen
par(
  mfrow = c(1, 1)
)




#Beispiel Einzelenen Sample größer darstellen
#Beispiel new_1
plot(
  bfast_results$new_1,
  main = "BFAST Lite - new_1",
  xlab = "Jahr",
  ylab = "NDVI"
)




#Breakpoints aller 9 Samples nun als Tabelle ausgeben
#Breakpoints ausgeben
get_break_dates <- function(model, sample_name, sample_type) {
  
  # LWZ-Lösung explizit auswählen
  bp_result <- strucchangeRcpp::breakpoints(
    model$breakpoints,
    breaks = "LWZ"
  )
  
  bp <- bp_result$breakpoints
  
  
  # Falls kein Breakpoint erkannt wurde
  if (length(bp) == 1 && is.na(bp)) {
    
    return(
      data.frame(
        id = sample_name,
        type = sample_type,
        break_date = as.Date(NA)
      )
    )
  }
  
  
  # Zeitpunkt aus den von BFAST Lite vorbereiteten Daten holen
  break_time <- model$data_pp$time[bp]
  
  break_year <- floor(
    break_time
  )
  
  break_month <- round(
    (break_time - break_year) * 12
  ) + 1
  
  
  # Datum erzeugen
  break_date <- as.Date(
    sprintf(
      "%04d-%02d-01",
      break_year,
      break_month
    )
  )
  
  
  data.frame(
    id = sample_name,
    type = sample_type,
    break_date = break_date
  )
}


# Leere Liste für Ergebnisse
break_list <- list()

for (sample_name in names(bfast_results)) {
  
  sample_type <- as.character(
    sample_list[[sample_name]]$type[1]
  )
  
  break_list[[sample_name]] <- get_break_dates(
    bfast_results[[sample_name]],
    sample_name,
    sample_type
  )
}


# Ergebnisse zu einer Tabelle zusammenführen
break_table <- bind_rows(
  break_list
)


# Tabelle anzeigen
break_table



#Ergebnisse als PNG speichern
png(
  filename = "BFAST_Lite_NDVI_2018_2025.png",
  width = 3600,
  height = 2400,
  res = 300
)

par(
  mfrow = c(3, 3),
  mar = c(4, 4, 2.5, 1)
)

for (sample_name in names(bfast_results)) {
  
  plot(
    bfast_results[[sample_name]],
    breaks = "LWZ",
    main = sample_name,
    xlab = "Jahr",
    ylab = "NDVI"
  )
}

dev.off()

# Plot-Einstellungen wieder zurücksetzen
par(
  mfrow = c(1, 1)
)


#auch NDVI Plots saven
ndvi_plot <- ggplot(
  time_series,
  aes(
    x = date,
    y = NDVI
  )
) +
  geom_line() +
  facet_wrap(
    ~ id,
    ncol = 3
  ) +
  labs(
    title = "Monatliche NDVI-Zeitreihen 2018-2025",
    x = "Jahr",
    y = "NDVI"
  ) +
  theme_minimal()

ggsave(
  filename = "NDVI_Zeitreihen_2018_2025.png",
  plot = ndvi_plot,
  width = 12,
  height = 8,
  units = "in",
  dpi = 300
)