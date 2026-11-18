#' @import data.table
#' @import forcats

# Gesamte Recodierungsfunktion
rec_pp <- function(data) {
  data <- rec_schultyp(data)
  data <- rec_ausbildungsfeld(data)
  data <- rec_schulform(data)
  data <- rec_geschlecht(data)
  data <- rec_geschlecht_feld(data)
  data <- rec_bundesland(data)
  data <- rec_betrieb(data)
  data <- rec_sprache(data)
  data <- rec_ort(data)
  data <- rec_plaene(data)
  data <- rec_entsprechung(data)
  data <- rec_unklarheiten(data)
  data <- rec_stunden(data)
  data <- rec_beschaeftigungsangebot(data)
  data <- rec_berufsbereich(data)
  data <- rec_bildungshintergrund(data)
  data <- rec_q17(data)
  data <- rec_q45(data)
  data <- rec_cluster(data)
  return(data)
}

# Erstelle Breaksvariablen ----
## Erstelle Schultyp ----
rec_schultyp <- function(data) {
  data[,
    SCHULTYP := fct_recode(
      Q01,
      "BMS" := "Fachschule, Handelsschule (ohne Matura)",
      "BHS" := "Höhere Lehranstalt, Handelsakademie (mit Matura)"
    )
  ]
  setattr(data$SCHULTYP, "label", "Schultyp")
  return(data)
}

## Erstelle Ausbildungsfeld ----
rec_ausbildungsfeld <- function(data) {
  data[,
    AUSBILDUNGSFELD := fct_recode(
      Q02,
      "Bildungsanstalt pädagogisch" := "Bildungsanstalt/Schule für pädagogische Berufe (Elementar-, Sonderpädagogik pädagogische Assistenz)",
      "Kaufmännische Schule" := "Kaufmännische Schule (HAK, HAS)",
      "Soziales" := "Schule für Sozialberufe",
      "Technisch-gewerbliche Schule" := "Technisch-gewerbliche Schule",
      "Tourismus" := "Schule für Tourismus",
      "Wirtschaftliche Schule" := "Schule für wirtschaftliche Berufe (ohne Tourismus und Sozialberufe)"
    )
  ]
  setattr(data$AUSBILDUNGSFELD, "label", "Ausbildungsfeld")
  return(data)
}

# Recoode Schulform ----
## Kombination Schultyp x Ausbildungsfeld ----
rec_schulform <- function(data) {
  data[,
    SCHULFORM := fct_cross(SCHULTYP, AUSBILDUNGSFELD, sep = "|") |>
      fct_recode(
        "BHS (kaufm.)" = "BHS|Kaufmännische Schule",
        "BMS (kaufm.)" = "BMS|Kaufmännische Schule",
        "BHS (techn.)" = "BHS|Technisch-gewerbliche Schule",
        "BMS (techn.)" = "BMS|Technisch-gewerbliche Schule",
        "BHS (wirtsch.)" = "BHS|Wirtschaftliche Schule",
        "BMS (wirtsch.)" = "BMS|Wirtschaftliche Schule",
        "BHS (tourist.)" = "BHS|Tourismus",
        "BMS (tourist.)" = "BMS|Tourismus",
        "BHS (pädag.)" = "BHS|Bildungsanstalt pädagogisch",
        "BMS (pädag.)" = "BMS|Bildungsanstalt pädagogisch",
        "BHS (sozial)" = "BHS|Soziales",
        "BMS (sozial)" = "BMS|Soziales"
      ) |>
      fct_relevel(
        "BHS (kaufm.)",
        "BMS (kaufm.)",
        "BHS (techn.)",
        "BMS (techn.)",
        "BHS (wirtsch.)",
        "BMS (wirtsch.)",
        "BHS (tourist.)",
        "BMS (tourist.)",
        "BHS (pädag.)",
        "BMS (pädag.)",
        "BHS (sozial)",
        "BMS (sozial)"
      )
  ]
  setattr(data$SCHULFORM, "label", "Schulform")
  return(data)
}

# Erstelle Geschlecht ----
rec_geschlecht <- function(data) {
  data[,
    GESCHLECHT := fct_recode(
      Q59,
      "Männlich" = "Männlich",
      "Weiblich" = "Weiblich",
      "Divers" = "Divers"
    )
  ]
  return(data)
}

## Kombination Geschlecht x Ausbildungsfeld ----
rec_geschlecht_feld <- function(data) {
  data[,
    GESCHLECHT_FELD := fct_cross(GESCHLECHT, AUSBILDUNGSFELD, sep = "|") |>
      fct_recode(
        "Männlich - Pädagogisch" = "Männlich|Bildungsanstalt pädagogisch",
        "Männlich - Kaufmännisch" = "Männlich|Kaufmännische Schule",
        "Männlich - Sozial" = "Männlich|Soziales",
        "Männlich - Technisch-gewerblich" = "Männlich|Technisch-gewerbliche Schule",
        "Männlich - Tourismus" = "Männlich|Tourismus",
        "Männlich - Wirtschaftlich" = "Männlich|Wirtschaftliche Schule",
        "Weiblich - Pädagogisch" = "Weiblich|Bildungsanstalt pädagogisch",
        "Weiblich - Kaufmännisch" = "Weiblich|Kaufmännische Schule",
        "Weiblich - Sozial" = "Weiblich|Soziales",
        "Weiblich - Technisch-gewerblich" = "Weiblich|Technisch-gewerbliche Schule",
        "Weiblich - Tourismus" = "Weiblich|Tourismus",
        "Weiblich - Wirtschaftlich" = "Weiblich|Wirtschaftliche Schule",
        "Divers - Pädagogisch" = "Divers|Bildungsanstalt pädagogisch",
        "Divers - Kaufmännisch" = "Divers|Kaufmännische Schule",
        "Divers - Sozial" = "Divers|Soziales",
        "Divers - Technisch-gewerblich" = "Divers|Technisch-gewerbliche Schule",
        "Divers - Tourismus" = "Divers|Tourismus",
        "Divers - Wirtschaftlich" = "Divers|Wirtschaftliche Schule"
      )
  ]
  setattr(data$GESCHLECHT_FELD, "label", "Geschlecht - Richtung")
  return(data)
}

# Erstelle Bundesland ----
rec_bundesland <- function(data) {
  data[,
    BUNDESLAND := fct_recode(
      BL,
      "Burgenland" = "Burgenland",
      "Kärnten" = "Kärnten",
      "Niederösterreich" = "Niederösterreich",
      "Oberösterreich" = "Oberösterreich",
      "Salzburg" = "Salzburg",
      "Steiermark" = "Steiermark",
      "Tirol" = "Tirol",
      "Vorarlberg" = "Vorarlberg",
      "Wien" = "Wien"
    )
  ]
  setattr(data$BUNDESLAND, "label", "Bundesland Schule")
  return(data)
}

# Erstelle Betriebsgröße ----
rec_betrieb <- function(data) {
  data[,
    BETRIEBSGROESSE := fct_recode(
      Q28,
      "1-4 MA" = "1-4 Mitarbeiter:innen",
      "5-9 MA" = "5-9 Mitarbeiter:innen",
      "10-49 MA" = "10-49 Mitarbeiter:innen",
      "50-99 MA" = "50-99 Mitarbeiter:innen",
      "100-250 MA" = "100-250 Mitarbeiter:innen",
      "über 250 MA" = "Über 250 Mitarbeiter:innen",
      "Weiß nicht" = "Weiß nicht"
    ) |>
      fct_relevel(
        "1-4 MA",
        "5-9 MA",
        "10-49 MA",
        "50-99 MA",
        "100-250 MA",
        "über 250 MA",
        "Weiß nicht"
      )
  ]
  setattr(data$BETRIEBSGROESSE, "label", "Größe Betrieb")
  return(data)
}

# Erstelle Sprache ----
rec_sprache <- function(data) {
  data[,
    SPRACHE_DE := fct_recode(
      Q61,
      "Nur Deutsch" = "Nur Deutsch",
      "Deutsch und andere Sprache(n)" = "Überwiegend Deutsch und andere Sprache(n)",
      "Andere Sprache(n) und Deutsch" = "Überwiegend andere Sprache(n) und Deutsch",
      "Nur andere Sprache(n) als Deutsch" = "Andere Sprache(n) als Deutsch"
    )
  ]
  setattr(data$SPRACHE_DE, "label", "Sprache Zuhause")
  return(data)
}

# Erstelle Ort des Praktikums ----
rec_ort <- function(data) {
  data[,
    PRAKTIKUMSORT := fct_recode(
      Q24,
      "Regional" = "Regional, in der Nähe (eigene Gemeinde)",
      "Überregional" = "Überregional, weiter weg (z.B. andere Gemeinde, anderes Bundesland)",
      "Im Ausland" = "Im Ausland"
    ) |>
      fct_relevel("Regional", "Überregional", "Im Ausland")
  ]
  setattr(data$PRAKTIKUMSORT, "label", "Ort des Praktikums")
  return(data)
}

# Erstelle Pläne nach der Ausbildung ----
rec_plaene <- function(data) {
  data[,
    PLAENE := fct_recode(
      Q57,
      "Suche nach dem Arbeitsplatz" = "Ich werde mir einen Arbeitsplatz suchen.",
      "Weiterführende Ausbildung" = "Ich werde eine weiterführende Ausbildung beginnen.",
      "Weiß noch nicht genau" = "Weiß noch nicht genau"
    ) |>
      fct_relevel(
        "Suche nach dem Arbeitsplatz",
        "Weiterführende Ausbildung",
        "Weiß noch nicht genau"
      )
  ]
  setattr(data$PLAENE, "label", "Pläne nach Ausbildung")
  return(data)
}

# Erstelle Entsprechung Praktikum/Ausbildungsrichtung ----
rec_entsprechung <- function(data) {
  data[,
    ENTSPRECHUNG := fct_recode(
      TAET2,
      "Ja" = "Ja, das Praktikum entspricht vollständig meiner Ausbildungsrichtung",
      "Mit Ausbildungsrichtung verwandt" = "Ja, das Praktikum ist mit meiner Ausbildungsrichtung verwandt",
      "Nein" = "Nein, gar nicht",
      # leere Antwortkategorie wird zu NA
      NULL = ""
    ) |>
      fct_relevel("Ja", "Mit Ausbildungsrichtung verwandt", "Nein")
  ]
  setattr(
    data$ENTSPRECHUNG,
    "label",
    "Entsprechung Praktikum - Ausbildungsrichtung"
  )
  return(data)
}

# Erstelle Unklarheiten im Dienstvertrag ----
rec_unklarheiten <- function(data) {
  data[,
    UNKLARHEITEN := fct_recode(
      Q36A,
      "Ja" = "Ja, Bezahlung/Entgelt",
      "Ja" = "Ja, Arbeitszeit und Pausen",
      "Ja" = "Ja, Urlaub",
      "Ja" = "Ja, Aufgabenbereich und Tätigkeit",
      "Ja" = "Ja, Sonstiges",
      "Nein" = "Nein, es gab keine Unklarheiten",
      # leere Antwortkategorie wird zu NA
      NULL = ""
    ) |>
      fct_relevel("Ja", "Nein")
  ]
  setattr(data$UNKLARHEITEN, "label", "Unklarheiten im Dienstvertrag")
  return(data)
}

# Erstelle Arbeitsstunden pro Woche ----
rec_stunden <- function(data) {
  data[,
    ARBEITSSTUNDEN := cut(
      suppressWarnings(as.numeric(Q31)),
      breaks = c(-Inf, 20, 35, 40, 60, 80, Inf),
      labels = c(
        "bis 20 Stunden",
        "21-35 Stunden",
        "36-40 Stunden",
        "41-60 Stunden",
        "61-80 Stunden",
        "Mehr als 81 Stunden"
      )
    )
  ]
  setattr(
    data$ARBEITSSTUNDEN,
    "label",
    "Arbeitsstunden pro Woche (Durchschnitt)"
  )
  return(data)
}

# Erstelle Beschäftigungsangebot nach der Ausbildung ----
rec_beschaeftigungsangebot <- function(data) {
  data[,
    BESCHAEFTIGUNGSANGEBOT := fct_recode(
      Q55,
      "Ja" = "Ja",
      "Nein" = "Nein"
    ) |>
      fct_relevel("Ja", "Nein")
  ]
  setattr(
    data$BESCHAEFTIGUNGSANGEBOT,
    "label",
    "Betriebliches Beschäftigungsangebot nach Ausbildung"
  )
  return(data)
}

# Erstelle Bildungshintergrund der Eltern ----
# Höchster formaler Abschluss von Vater (Q64A) oder Mutter (Q64B),
# Kategorisierung analog zu PP2 (q59) und PP3 (q64).
rec_bildungshintergrund <- function(data) {
  # Rangfolge der Abschlüsse (1 = niedrigste, 5 = höchste Stufe)
  rang_abschluss <- c(
    "Pflichtschule (Hauptschule, Neue Mittelschule, Polytechnische Schule etc.)" = 1,
    "Lehre, Berufsschule" = 2,
    "Berufsbildende mittlere Schule (Fachschule, Handelsschule, Schule für Sozialbetreuungsberufe etc.)" = 3,
    "AHS-Matura (Gymnasium)" = 4,
    "BHS-Matura (HTL, HAK, HBLA, BAfEP/BASOP, Berufsreifeprüfung)" = 4,
    "Universität, Fachhochschule, Pädagogische Hochschule, Akademie" = 5
  )
  kategorien <- c("Niedrig", "Mittel", "Mittel", "Gehoben", "Hoch")

  data[,
    # "Weiß nicht" und leere Angaben sind nicht in rang_abschluss enthalten und
    # werden dadurch zu NA; bekanntes Elternteil zählt, wenn das andere fehlt
    BILDUNGSHINTERGRUND := {
      rang <- pmax(
        rang_abschluss[as.character(Q64A)],
        rang_abschluss[as.character(Q64B)],
        na.rm = TRUE
      )
      rang[is.infinite(rang)] <- NA
      factor(
        kategorien[rang],
        levels = c("Niedrig", "Mittel", "Gehoben", "Hoch")
      )
    }
  ]
  setattr(data$BILDUNGSHINTERGRUND, "label", "Familiärer Bildungshintergrund")
  return(data)
}

# Erstelle Berufsbereich des Praktikums ----
rec_berufsbereich <- function(data, min_n = 50) {
  data[,
    # Ausprägungen unterhalb der Schwelle werden zusammengefasst
    BERUFSBEREICH := fct_lump_min(
      Q27,
      min = min_n,
      other_level = "Sonstige Berufsbereiche"
    )
  ]
  return(data)
}

# Erstelle Multiple-Choice-Batterie Q17 ----
# "Warum war es aus Ihrer Sicht schwierig eine Stelle zu finden?" (Filter: Q16)
rec_q17 <- function(data, items = paste0("Q17_", 1:14)) {
  # rekodierte Variablen verwerfen, damit die Funktion wiederholbar bleibt
  alte_cols <- intersect(c("Q17_OTHER_REC", "Q17_NOANSWER"), names(data))
  if (length(alte_cols) > 0) {
    data[, (alte_cols) := NULL]
  }

  # leerer Text bei "Sonstiges" wird als "nicht gewählt" gewertet
  data[is.na(Q17_OTHER), Q17_OTHER := ""]
  data <- databand2::create_mc_var(
    data,
    var_names = items,
    var_other = "Q17_OTHER",
    vec_true = "Ja",
    vec_false = "Nicht Gewählt"
  )
  # create_mc_var hängt an "Sonstiges" ein "_rec" in Kleinschreibung an
  setnames(data, "Q17_OTHER_rec", "Q17_OTHER_REC")
  setattr(data$Q17_OTHER_REC, "label", attr(data$Q17_OTHER, "label"))

  # Q17 ist eine Filterfrage nach Q16: außerhalb des Filters sind die Items
  # nicht gültig (create_mc_var kann den Filter nur auf Zeichenvektoren anwenden)
  data[!Q16 %in% c("Schwierig", "Sehr schwierig"), (items) := NA]
  data[
    !Q16 %in% c("Schwierig", "Sehr schwierig"),
    Q17_OTHER_REC := NA_character_
  ]

  # "Keine Antwort": alle Items und "Sonstiges" nicht gewählt
  item_cols <- c(items, "Q17_OTHER_REC")
  data[,
    Q17_NOANSWER := {
      all_na <- Reduce(`&`, lapply(.SD, is.na))
      any_yes <- Reduce(
        `|`,
        lapply(.SD, \(x) !is.na(x) & as.character(x) == "Ja")
      )
      fcase(
        all_na  , NA_character_   ,
        any_yes , "Nicht Gewählt" ,
        default = "Ja"
      )
    },
    .SDcols = item_cols
  ]
  data[,
    Q17_NOANSWER := factor(Q17_NOANSWER, levels = c("Ja", "Nicht Gewählt"))
  ]
  # Label im Batterie-Format, damit tableband_mc() das Item ausweist
  setattr(
    data$Q17_NOANSWER,
    "label",
    "[Keine Antwort] Warum war es aus Ihrer Sicht schwierig eine Stelle zu finden?"
  )
  return(data)
}

# Erstelle numerische Ratingskalen für Q45 ----
# Schulnotenskala 1 (sehr gut) bis 5 (nicht genügend); die Originalfaktoren
# bleiben erhalten, die numerischen Varianten erhalten das Suffix "_NUM"
rec_q45 <- function(data, items = paste0("Q45_", 1:11)) {
  for (item in items) {
    num_var <- paste0(item, "_NUM")
    data[, (num_var) := scale_num(get(item))]
    setattr(data[[num_var]], "label", attr(data[[item]], "label"))
  }
  return(data)
}
