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
#Daten sortieren
time_series <- time_series %>%
  arrange(id, date)


#Grundlegende Kontrolle

# Anzahl aller Zeilen
nrow(time_series)

# Anzahl Beobachtungen pro Sample
table(time_series$id)

colSums(is.na(time_series))

# Zuordnung ID <-> Typ
table(
  time_series$id,
  time_series$type
)

# Zeitraum kontrollieren
range(time_series$date)


#Funktionen für BFast erstellen
run_bfast <- function(data_sample) {
  
  # Nach Datum sortieren
  data_sample <- data_sample %>%
    arrange(date)
  
  # NDVI als monatliche Zeitreihe
  ndvi_ts <- ts(
    data_sample$NDVI,
    start = c(2018, 1),
    frequency = 12
  )
  
  # BFAST Lite
  model <- bfastlite(
    ndvi_ts,
    formula = response ~ trend + harmon,
    order = 3,
    breaks = "LWZ",
    na.action = na.omit
  )
  
  return(model)
}


#alle 9 Samples aufteilen
sample_list <- split(
  time_series,
  time_series$id
)


#BFASt auf alle Samples anwenden
bfast_results <- lapply(
  sample_list,
  run_bfast
)

#Visuell automatisch plotten
for (sample_name in names(bfast_results)) {
  
  plot(
    bfast_results[[sample_name]],
    main = paste(
      "BFAST –",
      sample_name
    )
  )
  
}


#kleiner Vegleich mit bfast lite an new_1

# Sample auswählen und chronologisch sortieren
bfast_sample <- time_series %>%
  filter(id == "new_1") %>%
  arrange(date)


# Kontrolle:
# Bei vollständiger Zeitreihe sollten es 96 Monate sein
nrow(bfast_sample)

# Fehlende NDVI-Werte prüfen
sum(is.na(bfast_sample$NDVI))

# Monatliche NDVI-Reihe als ts-Objekt

bfast_sample_ts <- ts(
  bfast_sample$NDVI,
  start = c(2018, 1),
  frequency = 12
)


# Normale Zeitreihe zunächst ansehen
plot(
  bfast_sample_ts,
  main = "NDVI-Zeitreihe – new_1",
  xlab = "Jahr",
  ylab = "NDVI"
)


# BFAST Lite
bfast_sample_model <- bfastlite(
  bfast_sample_ts,
  formula = response ~ trend + harmon,
  order = 3,
  breaks = "LWZ",
  na.action = na.omit
)


# BFAST-Lite-Ergebnis anzeigen
plot(
  bfast_sample_model,
  main = "BFAST Lite – new_1"
)


# Modellinformationen
bfast_sample_model


# Erkannte Breakpoints ausgeben
bfast_sample_model$breakpoints$breakpoints

