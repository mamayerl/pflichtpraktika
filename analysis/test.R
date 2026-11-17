#################################################
## Hauptskript zur Erstellung des Tabellenbandes
# für das Pflichtpraktikum
#################################################

# Lade Libraries ----

rm(list = ls())
devtools::load_all()
library(haven)
library(databand2)

# Setze globale Variable für die Tabellenerstellung ----
options(databand2.count_digits = 0) # case numbers, default: 0
options(databand2.perc_digits = 2) # percentages, default: 1

# Lade Daten ----
set.seed(123)
PP <- haven::read_sav("data/stand0911 mit klein.sav") |>
    as_factor() |>
    setDT()

# Alle Spaltennamen in großbuchstaben umwandeln
setnames(PP, toupper(names(PP)))

# Filter nach vollständigen Fällen (lastpage >= 15) ----
PP <- PP[LASTPAGE >= 15]

# Erstelle Fake-Gewichte für die Analyse (werden später durch echte Gewichte
# ersetzt) ----
PP[, GEWICHT := runif(.N, min = 0.2, max = 4)]

# Rufe die Recodierungsfunktionen auf ----
PP <- rec_pp(PP)

# Definiere die Breakvariablen für die Analyse ----
breaks <- c(
    "SCHULTYP",
    "SCHULFORM",
    "GESCHLECHT",
    "GESCHLECHT_FELD",
    "AUSBILDUNGSFELD",
    "BUNDESLAND",
    "BETRIEBSGROESSE",
    "SPRACHE_DE",
    "BILDUNGSHINTERGRUND",
    "PRAKTIKUMSORT",
    "PLAENE",
    "ENTSPRECHUNG",
    "UNKLARHEITEN",
    "ARBEITSSTUNDEN",
    "BESCHAEFTIGUNGSANGEBOT",
    "CLUSTER_QUALITAET_3",
    "BERUFSBEREICH"
)

# Erstelle eine Beispieltabelle für die Variable Q02 (Ausbildungsfeld) ----
tab1 <- databand2::tableband_bi(
    PP,
    row_vars = breaks,
    col_vars = "Q02",
    weights = "GEWICHT",
    summary = F
)


PP[, .N, keyby = .(SPRACHE_DE, Q61)]

# Kontrolle der Clusterlösungen ----
PP[, .N, keyby = CLUSTER_QUALITAET_4]

# Mittelwertsprofil der Items je Cluster (1 = sehr gut)
PP[,
    lapply(.SD, \(x) mean(scale_num(x), na.rm = TRUE)),
    keyby = CLUSTER_QUALITAET_4,
    .SDcols = paste0("Q45_", 1:10)
]

# Plausibilitätscheck: Cluster gegen Gesamturteil
PP[, .N, keyby = .(CLUSTER_QUALITAET_4, Q45_11)]
