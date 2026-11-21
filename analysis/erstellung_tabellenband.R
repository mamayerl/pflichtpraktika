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
    #"GESCHLECHT",
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
Q02_bivariat <- databand2::tableband_bi(
    PP,
    row_vars = breaks,
    col_vars = "BILDUNGSHINTERGRUND",
    weights = "GEWICHT",
    summary = TRUE
)

# Beispieltabelle für die Multiple-Choice-Batterie Q17 (Hilfsmittel der Suche) ----
# Itemvektoren inkl. rekodiertem "Sonstiges" und Flag für "keine Antwort"
Q17_items <- c(paste0("Q17_", 1:14), "Q17_OTHER_REC", "Q17_NOANSWER")

Q17_multiplechoice <- tableband_mc(
    PP,
    row_vars = breaks,
    col_vars = Q17_items,
    count_factor = "Ja",
    weight = "GEWICHT",
    n_weighted = FALSE
)

# Mittelwerttabelle der Qualitätsbewertungen Q45 (1 = sehr gut) ----
q45_items <- paste0("Q45_", 1:11, "_NUM")

Q45_Mittelwert <- tableband_mean(
    PP,
    row_vars = breaks,
    col_vars = q45_items,
    weight = "GEWICHT",
    n_weighted = FALSE,
    item_labels = TRUE,
    item_sep = "]",
    item_label = "left",
    char_subst = "[",
    summary = TRUE,
    var_labels = TRUE
)

# Write Excel ----
# tableband_bi() liefert eine benannte Liste von Tabellen (eine je col_var)
tabs <- c(
    Q02_bivariat,
    list(Q17_multiplechoice, Q45_Mittelwert)
)
names(tabs) <- c("Q02_bivariat", "Q17_multiplechoice", "Q45_Mittelwert")

openxlsx::write.xlsx(
    tabs,
    "PP4_proto_tabellenband.xlsx",
    overwrite = TRUE
)


# Test
PP[, .N, keyby = .(Q17_1, SCHULTYP)]
PP[, .N, keyby = .(Q17_2, SCHULTYP)]


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
