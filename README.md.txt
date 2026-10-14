# Bachelorarbeit: Flächenmonitoring im New-Delta-Projekt

Dieses Repository enthält die Skripte meiner Bachelorarbeit im Studiengang
Geoinformatik an der Universität Münster:

„Teilautomatisiertes Flächenmonitoring agroindustrieller Bewässerungsareale
in hyperariden Regionen mittels multispektraler Satellitendaten und der
Google Earth Engine – am Beispiel des New-Delta-Projekts (Ägypten).“

Untersucht wird die räumliche und zeitliche Entwicklung landwirtschaftlicher
Flächen zwischen 2018 und 2025. Der Arbeitsablauf verbindet die Verarbeitung
von Sentinel-2-Daten in Google Earth Engine (GEE) mit weiterführenden
Auswertungen in R.

## Aufbau des Arbeitsablaufs

1. Aufbereitung der Sentinel-2-Daten und Berechnung der Jahresprädiktoren.
2. Vergleich von Random Forest und Support Vector Machine mit und ohne
   GLCM-Texturmerkmale für 2025.
3. Anwendung des RF-Modells auf die Jahre 2018–2025.
4. Vergleich aufeinanderfolgender Jahresklassifikationen und Berechnung
   der Veränderungsflächen in R.
5. Erstellung monatlicher NDVI-Zeitreihen für neun ausgewählte
   50-m-Pufferflächen und Strukturbruchanalyse mit BFAST Lite.

## GEE-Skripte

Die GEE-Skripte befinden sich im Ordner `bachelorthesis_GEE`:

1. ChangeDetection
2. Klassifikation
3. Zeitreihen


## R-Skripte (sind im Ordner 'Bachelorthesis`)

1. Klassifikationen
2. Klassifkationen_2024
3. RF_ChangeDetection
4. pixelbasierte_Zeitreihen


## Aus R gespeicherte Ergebnisse:

1. Ordner 'ChangeDetection_Postklassifikation'
2. Ordner 'Karten'
3. BFAST_Lite_NDVI_2018_2025
4. BFAST_Lite_Standardausgabe
5. NDVI_BFAST_Breakpoints
6. NDVI_Zeitreihen_2018_2025
7. NDVI_Zeitreihen_mit_Breakpoints
8. Rf + GLCM Validierung
9. Rf Validierung


## Voraussetzungen und Eingabedaten

Für die Ausführung werden ein eingerichteter GEE-Zugang und eine
R-Installation mit den in den Skripten angegebenen Paketen benötigt.

Die R-Auswertungen verwenden aus GEE exportierte GeoTIFF- und CSV-Dateien.
Dazu gehören die Jahresklassifikationen, RGB-Komposite, Trainings- und
Validierungsstichproben sowie die monatlichen Zeitreihendaten.

Vor der Ausführung müssen die Dateipfade in den R-Skripten angepasst werden.
Für die GEE-Skripte müssen außerdem die verwendeten Untersuchungs- und
Referenzgeometrien sowie die Stichprobenstandorte verfügbar sein.

## Ausführung

Die GEE-Skripte werden im Earth Engine Code Editor ausgeführt. Die
benötigten Tabellen und Raster werden anschließend exportiert und
als Eingaben für die R-Skripte verwendet.

Die R-Skripte können nach Anpassung der Eingabepfade unabhängig für
die jeweilige Auswertung ausgeführt werden.

## Hinweise zur Interpretation

Die monatlichen Zeitreihen beschreiben räumliche Medianwerte innerhalb
von 50-m-Puffern, keine einzelnen Pixel.

Die Veränderungskarten zeigen Übergänge vorhergesagter Landbedeckungsklassen.
Sie bestätigen allein weder neue Feldanlagen noch dauerhafte Nutzungsaufgaben.
Die Analyse erlaubt keine Bestimmung von Bewässerungsmengen.

## Autorin

Lena Long  
Bachelorstudiengang Geoinformatik  
Universität Münster