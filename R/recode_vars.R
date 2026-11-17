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
