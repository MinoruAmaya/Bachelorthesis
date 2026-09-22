#pixelbasierte Zeitreihen
#handelt sich nicht um einzelne Pixelzeitreihe, sonderrn monatliche Medianwerte der 50-m-Pufferflächen

#Darstellungen:
#NDVI-Zeitreihen von 2018-2025 als reine Monatswerte
#NDVI-Zeitreihe mit Breakpoints Hauptdarstellung
# BFAST-Lite

library(tidyverse)
library(lubridate)
library(bfast)

#Zeitreihendaten einlesen:
time_series <- read.csv(
  "NewDelta_TimeSeries_2018_2025.csv"
)

#Datum und Reihenfolge vorbereiten

time_series$date <- as.Date(
  paste0(
    time_series$date,
    "-01"
  )
)


# Reihenfolge der neun Stichproben
sample_order <- c(
  "decline_1",
  "decline_2",
  "decline_3",
  "new_1",
  "new_2",
  "new_3",
  "stable_1",
  "stable_2",
  "stable_3"
)

#Bezeichnungen der Abbildungen
sample_labels <- c(
  decline_1 = "Rückgang 1",
  decline_2 = "Rückgang 2",
  decline_3 = "Rückgang 3",
  new_1 = "Neue Aktivität 1",
  new_2 = "Neue Aktivität 2",
  new_3 = "Neue Aktivität 3",
  stable_1 = "Dauerhaft aktiv 1",
  stable_2 = "Dauerhaft aktiv 2",
  stable_3 = "Dauerhaft aktiv 3"
)


#Bezeichnungen der Stichprobengruppen
group_labels <- c(
  detectedDecline = "Erkannter Aktivitätsrückgang",
  newActive = "Neu aktive Fläche",
  stableActive = "Dauerhaft aktive Fläche"
)

# Farben der Stichprobengruppen
group_colors <- c(
  "Erkannter Aktivitätsrückgang" = "#B2182B",
  "Neu aktive Fläche" = "#2166AC",
  "Dauerhaft aktive Fläche" = "#1B7837"
)


# Daten sortieren
time_series <- time_series %>%
  mutate(
    id = factor(
      id,
      levels = sample_order
    )
  ) %>%
  arrange(
    id,
    date
  )

#Grundlegende Kontrolle
# Anzahl aller Zeilen
nrow(time_series)

# Anzahl der Beobachtungen je Stichprobe
table(time_series$id)

# Zuordnung zwischen ID und Typ
table(
  time_series$id,
  time_series$type
)

# Fehlende Werte
colSums(
  is.na(time_series)
)

# Untersuchungszeitraum
range(time_series$date)

# Jede Stichprobe sollte 96 Monatswerte besitzen
stopifnot(
  all(
    table(time_series$id) == 96
  )
)


#Daten für die Abbildungen vorbereiten
plot_data <- time_series %>%
  mutate(
    sample = unname(
      sample_labels[as.character(id)]
    ),
    
    sample = factor(
      sample,
      levels = unname(sample_labels)
    ),
    
    group = unname(
      group_labels[as.character(type)]
    ),
    
    group = factor(
      group,
      levels = unname(group_labels)
    )
  )


#monatl. NDVI-Zeitreihen ohne BFAST
ndvi_plot <- ggplot(
  plot_data,
  aes(
    x = date,
    y = NDVI,
    color = group,
    group = id
  )
) +
  geom_line(
    linewidth = 0.55
  ) +
  geom_point(
    size = 0.65,
    alpha = 0.5
  ) +
  facet_wrap(
    ~ sample,
    ncol = 3
  ) +
  scale_color_manual(
    values = group_colors,
    drop = FALSE
  ) +
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = as.Date(
      c(
        "2018-01-01",
        "2025-12-31"
      )
    ),
    expand = expansion(
      mult = c(0.01, 0.02)
    )
  ) +
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.25
    )
  ) +
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  labs(
    title = "Monatliche NDVI-Zeitreihen ausgewählter Stichprobenflächen",
    subtitle = "Räumlicher Median innerhalb der 50-m-Puffer",
    x = "Jahr",
    y = "NDVI",
    color = "Stichprobengruppe"
  ) +
  theme_minimal(
    base_size = 11
  ) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = "grey35"
    )
  )


# Abbildung anzeigen
ndvi_plot


# Als PNG speichern
ggsave(
  filename = "NDVI_Zeitreihen_2018_2025.png",
  plot = ndvi_plot,
  width = 13,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)


#Funktion für BFAST Lite
run_bfast <- function(sample_data) {
  
  # Zeitlich sortieren
  sample_data <- sample_data %>%
    arrange(date)
  
  
  # Monatliche NDVI-Zeitreihe definieren
  ndvi_ts <- ts(
    sample_data$NDVI,
    start = c(2018, 1),
    frequency = 12
  )
  
  
  # BFAST Lite anwenden
  model <- bfastlite(
    ndvi_ts
  )
  
  
  return(model)
}


#Daten auf die neun Stcihproben aufteilen

sample_list <- split(
  time_series,
  time_series$id
)


# BFAST Lite auf alle Stichproben anwenden
bfast_results <- lapply(
  sample_list,
  run_bfast
)


# Namen der Modelle kontrollieren
names(bfast_results)


#BFAST Darstellung anzeigen
par(
  mfrow = c(3, 3),
  mar = c(4, 4, 2.5, 1),
  oma = c(0, 0, 3, 0)
)


for (sample_name in names(bfast_results)) {
  
  plot(
    bfast_results[[sample_name]],
    breaks = "LWZ",
    main = sample_labels[[sample_name]],
    xlab = "Jahr",
    ylab = "NDVI"
  )
}


mtext(
  "BFAST-Lite-Analyse der monatlichen NDVI-Zeitreihen",
  outer = TRUE,
  font = 2,
  cex = 1.3
)


# Plot-Einstellungen zurücksetzen
par(
  mfrow = c(1, 1),
  oma = c(0, 0, 0, 0)
)



#Einzelne Stichproben darstellen
# Wichtig: Auch hier die LWZ-Lösung verwenden
plot(
  bfast_results$new_1,
  breaks = "LWZ",
  main = "BFAST Lite – Neue Aktivität 1",
  xlab = "Jahr",
  ylab = "NDVI"
)


#Breakpoints auslesen
get_break_dates <- function(
    model,
    sample_name,
    sample_type
) {
  
  # LWZ-Lösung auswählen
  bp_result <- strucchangeRcpp::breakpoints(
    model$breakpoints,
    breaks = "LWZ"
  )
  
  bp <- bp_result$breakpoints
  
  
  # Kein Breakpoint erkannt
  if (
    length(bp) == 1 &&
    is.na(bp)
  ) {
    
    return(
      data.frame(
        id = sample_name,
        type = sample_type,
        break_date = as.Date(NA)
      )
    )
  }
  
  
  # Breakpoint-Zeitpunkte aus dem Modell übernehmen
  break_time <- model$data_pp$time[bp]
  
  break_year <- floor(
    break_time
  )
  
  break_month <- round(
    (break_time - break_year) * 12
  ) + 1
  
  
  # Absicherung des Monatswertes
  break_month <- pmin(
    pmax(
      break_month,
      1
    ),
    12
  )
  
  
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


#Breakpoints aller Stichproben zusammenführen
break_list <- list()


for (sample_name in names(bfast_results)) {
  
  sample_type <- as.character(
    sample_list[[sample_name]]$type[1]
  )
  
  break_list[[sample_name]] <- get_break_dates(
    model = bfast_results[[sample_name]],
    sample_name = sample_name,
    sample_type = sample_type
  )
}


break_table <- bind_rows(
  break_list
)


# Tabelle anzeigen
break_table


# Optional: als CSV speichern
write.csv(
  break_table,
  "BFAST_Breakpoints_2018_2025.csv",
  row.names = FALSE
)




#Breakpoints für ggplot vorbereiten
break_plot_data <- break_table %>%
  filter(
    !is.na(break_date)
  ) %>%
  mutate(
    sample = unname(
      sample_labels[as.character(id)]
    ),
    
    sample = factor(
      sample,
      levels = unname(sample_labels)
    )
  )


# Stichproben ohne erkannten Breakpoint
no_break_data <- plot_data %>%
  distinct(
    sample
  ) %>%
  anti_join(
    break_plot_data %>%
      distinct(sample),
    by = "sample"
  ) %>%
  mutate(
    label_date = as.Date("2018-04-01"),
    label_y = 0.96
  )



#Zeitreihendarstellung mit Breakpoints
ndvi_break_plot <- ggplot(
  plot_data,
  aes(
    x = date,
    y = NDVI,
    color = group,
    group = id
  )
) +
  
  # Beobachtete monatliche NDVI-Werte
  geom_line(
    linewidth = 0.55
  ) +
  
  geom_point(
    size = 0.65,
    alpha = 0.5
  ) +
  
  # Mit LWZ ausgewählte Breakpoints
  geom_vline(
    data = break_plot_data,
    aes(
      xintercept = as.numeric(break_date)
    ),
    inherit.aes = FALSE,
    color = "#E69F00",
    linewidth = 0.75,
    linetype = "dashed"
  ) +
  
  # Hinweis für Stichproben ohne Breakpoint
  geom_text(
    data = no_break_data,
    aes(
      x = label_date,
      y = label_y,
      label = "kein Breakpoint"
    ),
    inherit.aes = FALSE,
    hjust = 0,
    vjust = 1,
    color = "grey40",
    size = 3
  ) +
  
  facet_wrap(
    ~ sample,
    ncol = 3
  ) +
  
  scale_color_manual(
    values = group_colors,
    drop = FALSE
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y",
    limits = as.Date(
      c(
        "2018-01-01",
        "2025-12-31"
      )
    ),
    expand = expansion(
      mult = c(0.01, 0.02)
    )
  ) +
  
  scale_y_continuous(
    breaks = seq(
      0,
      1,
      by = 0.25
    )
  ) +
  
  coord_cartesian(
    ylim = c(0, 1)
  ) +
  
  labs(
    title = "Monatliche NDVI-Zeitreihen und strukturelle Breakpoints",
    subtitle = paste(
      "Gestrichelte Linien zeigen die mit BFAST Lite",
      "und dem LWZ-Kriterium ausgewählten Breakpoints"
    ),
    x = "Jahr",
    y = "NDVI",
    color = "Stichprobengruppe"
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(
      color = "grey90",
      linewidth = 0.3
    ),
    strip.text = element_text(
      face = "bold"
    ),
    legend.position = "bottom",
    legend.title = element_text(
      face = "bold"
    ),
    plot.title = element_text(
      face = "bold"
    ),
    plot.subtitle = element_text(
      color = "grey35"
    )
  )


# Abbildung anzeigen
ndvi_break_plot


# Als PNG speichern
ggsave(
  filename = "NDVI_Zeitreihen_mit_Breakpoints.png",
  plot = ndvi_break_plot,
  width = 13,
  height = 8,
  units = "in",
  dpi = 300,
  bg = "white"
)





#BFAST Darstellung als PNG speichern
png(
  filename = "BFAST_Lite_Standardausgabe.png",
  width = 3900,
  height = 2400,
  res = 300
)


par(
  mfrow = c(3, 3),
  mar = c(4, 4, 2.5, 1),
  oma = c(0, 0, 3, 0)
)


for (sample_name in names(bfast_results)) {
  
  plot(
    bfast_results[[sample_name]],
    breaks = "LWZ",
    main = sample_labels[[sample_name]],
    xlab = "Jahr",
    ylab = "NDVI"
  )
}


mtext(
  "BFAST-Lite-Analyse der monatlichen NDVI-Zeitreihen",
  outer = TRUE,
  font = 2,
  cex = 1.3
)


dev.off()


# Plot-Einstellungen abschließend zurücksetzen
par(
  mfrow = c(1, 1),
  oma = c(0, 0, 0, 0)
)



#3x3 Zeireihengrafik exportieren
ggsave(
  filename = "NDVI_BFAST_Breakpoints.png",
  plot = ndvi_break_plot,
  width = 16,
  height = 10,
  units = "cm",
  dpi = 300,
  bg = "white"
)

#
png(
  filename = "BFAST_Lite_NDVI_2018_2025.png",
  width = 1890,
  height = 1180,
  res = 300
)