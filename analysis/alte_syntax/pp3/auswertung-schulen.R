# +++++++++++++++++++++++++++++ # 
# Individualiserte Auswertung für Schulen 
# +++++++++++++++++++++++++++++ # 

# Laden ----
library(tidyverse)
library(rstudioapi)
library(flextable)
library(sjmisc)
library(sjlabelled)
library(janitor)
library(officer)
library(openxlsx)
library(doconv)
library(tidyr)
library(fs)

# lbl.var.df <- readRDS("Variablenabgleich_alt_neu.RData")

# vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/")
# # source(paste0(vorlagen.path,"functions_tabellenband.R"), encoding = "latin1")
# source(paste0(vorlagen.path,"R-Scripts/functions_praktisch.R"), encoding = "utf8")


# Einstellungen ----
aufbereiten <- F
neu.berechnen <- F
word <- F # sollen Worddokumente und pdfs prozduziert werden?
word.background <- F # Soll der obige Prozess im Hintergrund laufen?

# Funktionen ----
fnc.codebook <- function(data, var.col = "name", label.col = "label") {
  
  if (names(df) %>% str_detect(".+_.+_") %>% any){
    warning("Manche Variablennamen enthalten mehrere Unterstriche und eignen sich nicht, um Mehrfachantwortkategorien zu unterscheiden. Folgende Syntax zur Bereinigung wird vorgeschlagen: 
            names(df) <- names(df) %>% str_replace('(.+)(_)(.+_)','\\1\\3')")
  }
  
  cd <- data %>%
    get_label %>%  
    as.list %>% 
    as.data.frame %>% 
    t %>% 
    as.data.frame %>% 
    rownames_to_column %>% 
    as_tibble %>% 
    rename(!!sym(var.col) := rowname,
           !!sym(label.col) := V1) %>% 
    mutate(
      strng = name %>% str_extract("[a-z]\\d+"),
      ma.kategorie = label %>% str_extract( "(?<=\\[).+(?=\\])"),
      Frage = label %>% str_remove("\\[.+\\] "),
      analytisch = if_else(name %>% str_detect("other"),
                           F,
                           T),
      var.typ = if_else(name %>% str_detect("_"),
                        "Mehrfach",
                        "Kategorisch"),
      newvar = NA
    )
  
  if (any(cd$name == "ipaddr")) {
    cd[1:which(cd$name == "ipaddr"),"analytisch"] <- F
    cd[1:which(cd$name == "ipaddr"),"paradata"] <- T
    
  } else {
    cd[1:which(cd$name == "datestamp"),"analytisch"] <- F
    cd[1:which(cd$name == "datestamp"),"paradata"] <- T
    
  }
  
  return(cd)
}


# Tabellen erstellen ----

## Daten aufbereiten ----

if (aufbereiten) {
  df <- readRDS("pp_tb.RData")
  
  df <- cbind(df,
                 df %>%
                   select(ausbildungsfeld, matches("q05")) %>% 
                   rowwise() %>% 
                   mutate(Schule = paste0(c_across(everything()), collapse = " ") %>% 
                            str_remove_all(" NA")) %>% 
                   select(Schule))
  
  df <- df %>% 
    mutate(ausbildungsfeld_rec = case_when(ausbildungsfeld %>% str_detect("Bildungsanstalt") ~ "Pädagogisch",
                                           ausbildungsfeld %>% str_detect("Kaufmännisch") ~ "Kaufmännisch",
                                           ausbildungsfeld %>% str_detect("Sozial") ~ "Sozialberuflich",
                                           ausbildungsfeld %>% str_detect("Technisch") ~ "Technisch-(kunst)gewerblich",
                                           ausbildungsfeld %>% str_detect("Touris") ~ "Touristisch",
                                           ausbildungsfeld %>% str_detect("Wirtschaft") ~ "Wirtschaftlich") %>% 
             factor(levels = c("Kaufmännisch",
                               "Technisch-(kunst)gewerblich",
                               "Touristisch",
                               "Wirtschaftlich",
                               "Pädagogisch",
                               "Sozialberuflich")),
           g22_rec = case_when(g22_1 == "Ja" ~ "Eltern/Familie",
                               g22_2 == "Ja" ~ "Bekannte/Freundeskreis",
                               g22_3 == "Ja" ~ "Internet (z.B. Plattformen, Google)",
                               g22_4 == "Ja" ~ "Bekanntheit des Betriebes",
                               g22_5 == "Ja" ~ "Eigener Kontakt zum Betrieb",
                               g22_6 == "Ja" ~ "Schule/LehrerIn",
                               g22_7 == "Ja" ~ "Aufgrund eines früheren Praktikums",
                               g22_other %>% is.na == F ~ "Sonstiges") %>% 
             factor(levels = c("Eltern/Familie",
                               "Bekannte/Freundeskreis",
                               "Internet (z.B. Plattformen, Google)",
                               "Bekanntheit des Betriebes",
                               "Eigener Kontakt zum Betrieb",
                               "Schule/LehrerIn",
                               "Aufgrund eines früheren Praktikums",
                               "Sonstiges")))
  
  df$g22_rec <- set_label(df$g22_rec,
                             "Wie haben Sie den Praktikumsbetrieb gefunden?") 
  
  
  ## Auswahl der Variablen 
  cb <- fnc.codebook(df)
  
  schul.vars <- c("q16","q18_anz_bew_kat2","g20_1","g20_2","g22_rec",
                  "q23_1","q23_2","q23_3","q23_4","q23_5","q23_6","q23_7","q23_8","q23_9","q23_10","q23_11","q23_12","q23_13",
                  "q24","q26","q30","q32","q33_2","q34","q36","q38_einkommen_gesamt_kat","q40_kosten_gesamt_kat",
                  "q43","q44_4","q44_5","q45_1","q45_2","q45_7","q45_8","q46_SQ001","q46_SQ002","q46_SQ003","q49",
                  "q53_1","q53_2","q53_3","q53_4","q55","q27","cluster_qualitaet_3")
  
  lbl.schul.vars <- cb %>%
    filter(name %in% schul.vars) %>% 
    arrange(match(name, schul.vars)) %>% 
    mutate(Frage = case_when(Frage %>% str_detect("Wie beurteilen Sie rückblickend das Pflichtpraktikum") ~ "Wie beurteilen Sie rückblickend das Pflichtpraktikum? - ",
                             Frage %>% str_detect("Wie würden Sie das Lernen im Betrieb beschreiben?") ~ "Wie würden Sie das Lernen im Betrieb beschreiben? - ",
                             Frage %>% str_detect("Bitte bewerten Sie folgende Aussagen zu den Erfahrungen aus dem Pflichtpraktikum") ~ "Wie bewerten Sie Ihre Erfahrungen aus dem Pflichtpraktikum? - ",
                             Frage %>% str_detect("wie Sie die Unterstützung durch die Schule rund um das Pflichtpraktikum einschätzen") ~ "Wie schätzen Sie die Unterstützung durch die Schule ein? - ",
                             Frage %>% str_detect("Wie würden Sie folgende Aussagen bewerten?") ~ "Wie würden Sie folgende Aussagen bewerten? - ",
                             Frage %>% str_detect("Bitte bewerten Sie, ob folgende Aspekte zutreffen.") ~ "",
                             Frage %>% str_detect("kosten") ~ "Kosten gesamt (kategorisiert)",
                             Frage %>% str_detect("Wie würden Sie die Betreuung im Betrieb beurteilen?") ~ "Wie würden Sie die Betreuung im Betrieb beurteilen? - ",
                             Frage %>% str_detect("Qualität der Rahmenbedingungen") ~ "Qualität der Rahmenbedingungen",
                             # Frage %>% str_detect() ~ ,)
                             .default = Frage),
           Frage.neu = paste0(Frage, ma.kategorie) %>% str_remove_all("NA| - NA"))
  
  schule <-
    df %>% 
    group_by(q01_rec, ausbildungsfeld_rec) %>% 
    reframe(Schule) %>% 
    mutate(schule.key = paste0(q01_rec,"-",Schule)) %>% 
    mutate(BL = Schule %>% str_extract("Wien|Burgenland|Steiermark|Niederösterreich|Oberösterreich|Tirol|Vorarlberg|Kärnten|Salzburg"),
           Adresse = Schule %>% str_extract("(?<= - ).+")) %>%
    mutate(schule.key = paste(q01_rec, ausbildungsfeld_rec, BL, Adresse, sep = ";"),
           ausbildung_typ = paste(q01_rec, ausbildungsfeld_rec, sep = "-"))
  
  df <-
    df %>% 
    left_join(schule %>% select(Schule, schule.key, ausbildung_typ), by = "Schule",
              relationship = "many-to-many") %>% 
    as_tibble
  
  saveRDS(df, "pp_tb_schule.RData")
  # saveRDS(schule, "")
} else {
  pp_tb <- readRDS("pp_tb_schule.RData")
}


## Total Tabelle ----
if (aufbereiten) {
  total.tb <- tbl.band.fnc(pp_tb, var = lbl.schul.vars$name, brk = "total", weighted = T, weight = "gewicht")
  
  for (i in 1:length(schul.vars)) {
    
    total.tb[[i]]$Fragestellung <- lbl.schul.vars[i,"Frage.neu"] %>% unlist %>% as.vector# Fragestellung befüllen, um die Zeilen dann in Flextable zu kollabieren # Fragestellung befüllen, um die Zeilen dann in Flextable zu kollabieren
    
    total.tb[[i]] <-
      total.tb[[i]] %>% 
      rename(col = Kategorie,
             Anzahl = `n (gültig)`) %>%
      mutate(across(3:ncol(.), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ",")), # Spalten formatieren
             across(3:(ncol(.)-1), ~ paste0(.x, " %"))) %>% 
      pivot_longer(-c(1,2), names_to = "Kategorie") %>%  # pivot_longer und _wider verwendet, um Zeilen und spalten zu tauschen
      pivot_wider(names_from = col, values_from = value)
    
  }
  
  total.tb <- total.tb %>% 
    list_rbind() %>% 
    rename(Gesamt = `-`)
  
  saveRDS(total.tb, file = "../rueckmeldungen/rueckmeldungen_schulen/total.tb.RData")
} else {
  total.tb <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/total.tb.RData")
}


## Vergleichsrichtungen ----
if (aufbereiten) {
  vgl.schulen.tb <- tbl.band.fnc(pp_tb, var = lbl.schul.vars$name, brk = "ausbildungsfeld_rec", weighted = T, weight = "gewicht")
  
  for (i in 1:length(schul.vars)) {
    
    vgl.schulen.tb[[i]]$Fragestellung <- lbl.schul.vars[i,"Frage.neu"] %>% unlist %>% as.vector# Fragestellung befüllen, um die Zeilen dann in Flextable zu kollabieren # Fragestellung befüllen, um die Zeilen dann in Flextable zu kollabieren
    
    vgl.schulen.tb[[i]] <- vgl.schulen.tb[[i]] %>% 
      rename(col = Kategorie,
             Anzahl = `n (gültig)`) %>%
      mutate(across(3:ncol(.), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ",")), # Spalten formatieren
             across(3:(ncol(.)-1), ~ paste0(.x, " %"))) %>% 
      pivot_longer(-c(1,2), names_to = "Kategorie") %>%  # pivot_longer und _wider verwendet, um Zeilen und spalten zu tauschen
      pivot_wider(names_from = col, values_from = value)
    
  }
  
  vgl.schulen.tb <- vgl.schulen.tb %>% 
    list_rbind()
  
  saveRDS(vgl.schulen.tb, file = "../rueckmeldungen/rueckmeldungen_schulen/vgl.schulen.tb.RData")
} else {
  vgl.schulen.tb <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/vgl.schulen.tb.RData")
}



## Schulspezifisch Tabelle ----
# benötigt die längste Zeit, ca. 17'
if (aufbereiten) {
  schule.tb <- list()
  for (j in 1:length(pp_tb$Schule %>% unique)) {
    # for (j in 1) {
    
    schule.tb[[j]] <- tbl.band.fnc(pp_tb %>% filter(schule.key == unique(schule.key)[j]), # hier wird nach Schule == j gefiltert
                                   var = lbl.schul.vars$name, brk = "Schule", weighted = T, weight = "gewicht")
    
    for (i in 1:length(schul.vars)) {
      
      schule.tb[[j]][[i]]["Fragestellung"] <- lbl.schul.vars[i,"Frage.neu"] %>% unlist %>% as.vector# Fragestellung befüllen, um die Zeilen dann in Flextable zu kollabieren
      
      schule.tb[[j]][[i]] <-
        schule.tb[[j]][[i]] %>% 
        # mutate(Kategorie = unique(schule.key)[j]) %>% # benötigt, wenn BHS und BMS unterschieden werden sollen
        rename(col = Kategorie,
               Anzahl = `n (gültig)`) %>%
        mutate(across(3:ncol(.), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ",")), # Spalten formatieren
               across(3:(ncol(.)-1), ~ paste0(.x, " %"))) %>% 
        pivot_longer(-c(1,2), names_to = "Kategorie") %>%  # pivot_longer und _wider verwendet, um Zeilen und spalten zu tauschen
        pivot_wider(names_from = col, values_from = value)
      
    }
    
    schule.tb[[j]] <- schule.tb[[j]] %>% 
      list_rbind()
  }
  
  schule.tb[[1]] %>% 
    View
  
  saveRDS(schule.tb, file = "../rueckmeldungen/rueckmeldungen_schulen/schule.tb_neu.RData")
} else {
  schule.tb <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schule.tb.RData")
}


## Zusammenführen ----
if (aufbereiten) {
  zusammen <- list()
  # for (j in 1:3) {
  for (j in 1:length(pp_tb$Schule %>% unique)) {
    zusammen[[j]] <-  schule.tb[[j]] %>% 
      left_join(vgl.schulen.tb, by = c("Fragestellung","Kategorie")) %>% 
      left_join(total.tb, by = c("Fragestellung","Kategorie")) %>% 
      mutate(Schule = names(.)[3])
  }
  
  saveRDS(zusammen, file = "../rueckmeldungen/rueckmeldungen_schulen/zusammen_neu.RData")
} else {
  zusammen <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/zusammen.RData")
}


## Gesamtzeile einfüen ----
if (aufbereiten) {
  ### Gesamtzeile erzeugen ----
  gesamt.zeile <- list()
  for (j in 1:length(pp_tb$Schule %>% unique)) {
    
    gesamt.zeile[[j]] <-   bind_cols(data.frame(Fragestellung = "Gesamt", Kategorie = "-"),
                                     rbind( # fasst die Ns über Schule, Ausbildungsrichtungen udn Gesamt zusammen
                                       pp_tb %>% 
                                         count(Schule) %>%
                                         as_tibble %>% 
                                         rename(Kat = Schule) %>% 
                                         .[j,], # Auswahl der Schule
                                       pp_tb %>% 
                                         count(ausbildungsfeld_rec) %>% 
                                         as_tibble %>% 
                                         mutate(ausbildungsfeld_rec = ausbildungsfeld_rec %>% as.character) %>% 
                                         rename(Kat = ausbildungsfeld_rec),
                                       pp_tb %>% 
                                         mutate(total = "Gesamt") %>% 
                                         count(total) %>% 
                                         as_tibble %>% 
                                         rename(Kat = total)
                                     ) %>%
                                       pivot_wider(names_from = Kat, values_from = n)
                                     
    ) %>% 
      as_tibble %>% 
      mutate(
        across(3:ncol(.), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ",")), # Spalten formatieren
        across(everything(), ~ as.character(.x)),
        Schule = names(.)[3]
      ) %>% 
      rename(Schul.n = 3)
    
  }
  
  gesamt.zeile.1 <- gesamt.zeile %>%
    list_rbind
  
  ### Gesamtzeile einsetzen ----
  einsetzen <- list()
  for (j in 1:length(pp_tb$Schule %>% unique)) {
    
    ges.z <- gesamt.zeile.1[which(gesamt.zeile.1$Schule %in% (zusammen[[j]]$Schule %>% unique)),]
    names(ges.z)[3] <- ges.z$Schule
    n <- ges.z$Schule %>% # Schulnamen abändern
      str_replace(" - ",": ") %>% 
      str_replace("(?<=\\w)-(?=\\w)"," - ")
    
    einsetzen[[j]] <- rbind(ges.z,
                            zusammen[[j]]) %>% 
      select(-Schule) %>% 
      rename(!!sym(n) := 3)
    
  }
  
  names(einsetzen) <- schule %>% 
    str_replace("Soziales","Sozialberufliche Schule")
  
  for (t in which(names(einsetzen) %>% str_detect("Sozial"))) {
    
    n <- names(einsetzen[[t]]) %>% str_replace("Soziales","Sozialberufliche Schule")
    names(einsetzen[[t]]) <- n
    
  }
  
  saveRDS(einsetzen, file = "../rueckmeldungen/rueckmeldungen_schulen/einsetzen.RData")
} else {
  einsetzen <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/einsetzen.RData")
}


## Kleine Schulen aussortieren ----
einsetzen$`Kaufmännische Schule Steiermark - Eisenerz-Hieflauer Straße 89`
einsetzen %>% length
keep(einsetzen, ~ .x[1,3] > 15) %>% length
  
if (aufbereiten) {
  schulen.gefiltert <- keep(einsetzen, ~ .x[1,3] > 15)
  
  saveRDS(schulen.gefiltert, file = "../rueckmeldungen/rueckmeldungen_schulen/schulen_gefiltert.RData")
} else {
  schulen.gefiltert <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulen_gefiltert.RData")
}


if (aufbereiten) {  
  schulnamen.df <-
    schulen.gefiltert %>% 
    names %>% 
    str_replace("Wirtschaft.+Schule","Wirt_") %>% 
    str_replace("Touri.+Schule","Tour_") %>% 
    str_replace("Tourismus","Tour_") %>% 
    str_replace("Sozial.+Schule","Sozial_") %>% 
    str_replace("Techn.+Schule","Techn_") %>% 
    str_replace("Bildungs.+pädagogisch","Pädag_") %>% 
    str_replace("Kaufmännisch.+Schule","Kaufm_") %>% 
    str_replace("Wien(?= )","W") %>% 
    str_replace("Burgenland","B") %>% 
    str_replace("Kärnten","K") %>% 
    str_replace("Niederösterreich","NÖ") %>% 
    str_replace("Oberösterreich","OÖ") %>% 
    str_replace("Steiermark","Stk") %>% 
    str_replace("Salzburg","Sbg") %>% 
    str_replace("Tirol","T") %>% 
    str_replace("Vorarlberg","V") %>% 
    str_split_fixed(" - ", n = 2) %>% 
    as.data.frame() %>% 
    as_tibble %>% 
    rename(Art = 1,
           Adresse = 2) %>% 
    separate_wider_delim(Art, names = c("Art","BL"), delim = "_ ") %>% 
    mutate(Adresse = str_replace(Adresse, "-", "**")) %>% 
    separate_wider_delim(Adresse, names = c("Gemeinde", "Straße"), delim = "**") %>% 
    mutate(across(everything(), ~ .x %>% str_replace_all(" ","_")),
           Anschrift = paste(Art, BL, Gemeinde, Straße, sep = "-"))  
  
  saveRDS(schulnamen.df, file = "../rueckmeldungen/rueckmeldungen_schulen/schulnamen.df.RData")
} else {
  schulnamen.df <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulnamen.df.RData")  
}


# Flextables ----
if (aufbereiten) {
  # data <- einsetzen[[1]]
  fnc.flext <- function(data) {
    dims <- dim(data)
    # str_wrap column names to keep in limits for height of colnames
    names(data)[3:dims[2]] <- names(data)[3:dims[2]] %>% str_wrap(30)
    # borders
    borders <- data %>%
      group_by(Fragestellung) %>% 
      mutate(f = row_number(Fragestellung)) %>% 
      ungroup %>% 
      mutate(f = if_else(f == 1, row_number()-1, NA)) %>% 
      select(f) %>%
      slice(-1) %>% 
      na.omit %>% 
      unlist %>% 
      as.vector 
    # create flextable
    data %>% 
      flextable %>% 
      merge_v(j = 1, part = "body") %>% 
      rotate(i = 1, j = 3:dims[2], rotation = "btlr", part = "header") %>% 
      valign(valign = "center", part = "header") %>% 
      valign(j = 1:2, valign = "bottom", part = "header") %>%
      # valign(i = 1, j = 1, valign = "center", part = "body") %>% 
      align(i = 1, j = 2, align = "left", part = "header") %>% 
      # align(i = 1:3, j = 3:dims[2], align = "center", part = "header") %>% 
      align( j = 3:dims[2], align = "center", part = "header") %>%
      align(j = 3:dims[2], align = "right", part = "body") %>% 
      # align(i = 2, j = 1:2, align = "left", part = "header") %>% 
      bold( part = "header") %>%
      # bold(j = 1, part = "body") #%>%
      fontsize(size = 8, part = "all") %>% 
      font(fontname = "Calibri", part = "all") %>% 
      colformat_num(big.mark = ".", decimal.mark = ",") %>% 
      width(j = 1, width = c(4), unit = "cm") %>%
      width(j = 2, width = c(4), unit = "cm") %>%
      width(j = 3:dims[2], width = (17-8)/(dims[2]-2), unit = "cm") %>% # 17 = beschreibbare Seitenbreite; 7 = 1. + 2. Spaltenbreite
      # height(i = 2, height = 3, part = "header", unit = "cm") %>% 
      hrule(rule = "atleast", part = "all") %>% 
      height(height = 4, unit = "cm", part = "header") %>%
      height(height = 0.53, unit = "cm", part = "body") %>% 
      # bg(i = 4, j = NULL, "lightgrey", part = "header") %>% 
      bg(j = 3, bg="lightgrey", part = "body") %>% 
      bg(j = 3, bg="lightgrey", part = "header") %>% 
      bg(j = dims[2], bg="#F8F8F8", part = "body") %>% 
      border_outer(border = fp_border_default()) %>% 
      border_inner(border = fp_border_default(), part = "all") %>%
      # hline(i = 1, j = 1, border = fp_border(width = 0), part = "header") %>% 
      # hline_top(j = 1, border = fp_border(width = 0), part = "header") %>%
      paginate(group = "Fragestellung", group_def = "rle") %>% 
      padding(padding = 1, part = "all") %>% 
      padding(padding.right = 2.5, part = "all") %>% 
      # padding(i = 3, j = 1:2, padding.top = 90, padding.bottom = 2, part = "header") %>% 
      # padding(i = 1, padding.top = 10, padding.bottom = 2, part = "header") %>%
      hline(i = borders, border = fp_border(width = 1.8), part = "body") %>% 
      hline(border = fp_border(width = 1.8), part = "header")
    # padding(i = 3, j = 1:2, padding.top = 90, padding.bottom = 2, part = "header") %>% 
    # padding(i = 1, padding.top = 4, padding.bottom = 4, part = "header") %>% 
    # hline(i = borders, border = fp_border(width = 1.8), part = "body") %>% 
    # hline(border = fp_border(width = 1.8), part = "header")
    
  }
  
  ft.list <- map(schulen.gefiltert, fnc.flext)
  saveRDS(ft.list, file = "../rueckmeldungen/rueckmeldungen_schulen/ft.list.RData")
} else {
  ft.list <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/ft.list.RData")
  }


# Word erstellen ----
if (word) {
  
  if (word.background) {
    
    jobRunScript(
      "background.R",
      name = NULL,
      encoding = "latin1",
      workingDir = NULL,
      importEnv = FALSE,
      exportEnv = ""
    )
    
  } else {
    bold_face <- shortcuts$fp_bold()
    
    doc.list <- list()
    for (k in 1:length(ft.list)) {
      # for (k in 1:1) {
      
      read_docx(path = "Tabellenband_rueckmeldungen_template.docx") %>%
        body_add_par(value = "", style = "Normal") %>%
        body_add_par(value = "", style = "Normal") %>%
        body_add_par(value = "Erfahrungen mit Pflichtpraktika in den berufsbildenden Schulen", style = "Title") %>%
        body_add_par(value = "-", style = "Subtitle") %>%
        body_add_par(value = "Schulspezifische Rückmeldungen für den Schulstandort", style = "Subtitle") %>%
        # body_add_par(value = "-", style = "Subtitle") %>%
        body_add_par(value = names(schulen.gefiltert)[k], style = "Subtitle") %>%
        body_add_par(value = "Sehr geehrte Direktionen,", style = "Fließtext") %>%
        body_add_par(value = "das Österreichische Institut für Berufsbildungsforschung (öibf) bedankt sich sehr herzlich für die Mitwirkung Ihrer Schule an dieser Studie, die von der Arbeiterkammer Wien und dem Bundesministerium für Bildung, Wissenschaft und Forschung beauftragt wurde. Die Ergebnisse liegen jetzt in einer umfangreichen Berichtsform vor, die wir Ihnen hiermit gerne zur Verfüung stellen.", style = "Fließtext") %>%
        body_add_par(value = "Als speziellen Dank für Ihre Teilnahme dürfen wir Ihnen auch eine schulspezifische Auswertung übermitteln, in der Sie die Ergebnisse Ihrer Schule mit jenen der Gesamtergebnisse vergleichen können. Um die Anonymität nach den Schulstandorten zu gewährleisten, haben die Auftraggeber keine schulspezifische Auswertung erhalten. Die Ergebnisse dienen ausschließlich zu Ihrer internen Verwendung.", style = "Fließtext") %>%
        body_add_par(value = "", style = "Fließtext") %>%
        body_add_par(value = "", style = "Fließtext") %>%
        body_add_fpar(value = fpar(ftext("Hinweise zur Interpretation:", prop = bold_face)), style = "Fließtext") %>%
        body_add_par(value = 'In den Vergleichsgruppen finden sich Schulen für die groben Ausbildungsrichtungen "kaufmännisch", "technisch-gewerblich", "wirtschaftsberufliche", "touristisch", ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¾pädagogischÃƒÂ¢Ã¢â€šÂ¬Ã…â€œ und ÃƒÂ¢Ã¢â€šÂ¬Ã…Â¾sozialberuflichÃƒÂ¢Ã¢â€šÂ¬Ã…â€œ. Für genauere Informationen zur Stichprobe dürfen wir auf den Endbericht verweisen, der auf unserer Website http://www.oeibf.at frei abrufbar ist.', style = "List Bullet") %>%
        body_add_par(value = "Bei den schulspezifischen Auswertungen können die jeweiligen Stichproben sehr klein sein. Daraus ergeben sich erhebliche statistische Schwankungsbreiten. Die Ergebnisse können aus methodischer Sicht daher lediglich als Orientierungshilfe gesehen werden, um eventuelle Tendenzen abzuleiten.", style = "List Bullet") %>%
        body_add_par(value = 'Einige Fragen wurden als Filterfragen konzipiert. Dies spiegelt sich in einer hohen Anzahl von fehlenden Werten wider. Diese wurden mit "-" gekennzeichnet. Ein Prozentwert von "0 %" verweist auf einen gerundeten Wert kleiner als 0,5 %.', style = "List Bullet") %>%  
        body_add_par(value = "", style = "Fließtext") %>%
        body_add_par(value = "", style = "Fließtext") %>%
        body_add_fpar(value = fpar("Herzlichen Dank für Ihre Unterstützung, auch im Namen des Projektteams!", 
                                   run_linebreak(), 
                                   ftext("Dr. Norbert Lachmayr"),
                                   run_linebreak(), 
                                   ftext("öibf - Margaretenstraße 166/2. Stock, 1050 Wien; ZVR-Nr.: 718743404; www.oeibf.at")),
                      style = "Fließtext_li") %>%
        body_add_break(pos = "after") %>% 
        body_add_flextable(value = ft.list[[k]]) %>%
        print(target = paste0("../rueckmeldungen/rueckmeldungen_schulen/word/", schulnamen.df$Anschrift[k], ".docx"))
      
      docx2pdf(input = paste0("../rueckmeldungen/rueckmeldungen_schulen/word/", schulnamen.df$Anschrift[k], ".docx"),
               output = paste0("../rueckmeldungen/rueckmeldungen_schulen/pdfs/", schulnamen.df$Anschrift[k], ".pdf"))
      
    }
  }
}
  

# system("open ../rueckmeldungen/rueckmeldungen_schulen/KaufmännischeSchuleSteiermark-Eisenerz-HieflauerStraße89.docx")


# Emailadressen ----

path = "../rueckmeldungen/Schulstandorte.txt"
x <- readLines(path) %>% 
  as_tibble()
x %>% 
  print(n=50)

index <-
  x %>% 
  mutate(seq = seq_along(value),
         schule = if_else(value %>% str_detect("Details ansehen|Es gibt \\d+ Treffer zu Ihrer Suche\\."), seq+1, NA),
         email = if_else(value %>% str_detect("Details ansehen|Es gibt \\d+ Treffer zu Ihrer Suche\\."), seq+7, NA),
         adresse = if_else(value %>% str_detect("Details ansehen|Es gibt \\d+ Treffer zu Ihrer Suche\\."), seq+3, NA)) %>% 
  na.omit 

gesamtliste <-
  tibble(Schule = unlist(x)[index$schule],
         Adresse = unlist(x)[index$adresse],
         Email = unlist(x)[index$email])
gesamtliste %>% 
  print(n=50)


## Abgleich alt-neu ----
length(schulen.gefiltert)

schulen <-
schulen.gefiltert %>% 
  names %>% 
  str_split_fixed(" - ", n = 2) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  rename(Art = 1,
         Straße = 2) %>% 
  mutate(BL = case_when(Art %>% str_detect("Wien") ~ "Wien",
                        Art %>% str_detect("Burgenland") ~ "Burgenland",
                        Art %>% str_detect("Kärnten") ~ "Kärnten",
                        Art %>% str_detect("Niederösterreich") ~ "Niederösterreich",
                        Art %>% str_detect("Oberösterreich") ~ "Oberösterreich",
                        Art %>% str_detect("Steiermark") ~ "Steiermark",
                        Art %>% str_detect("Salzburg") ~ "Salzburg",
                        Art %>% str_detect("Tirol") ~ "Tirol",
                        Art %>% str_detect("Vorarlberg") ~ "Vorarlberg",
                        .default = NA),
         Art = str_remove(Art, " Wien| Burgenland| Kärnten| Niederösterreich| Oberösterreich| Steiermark| Salzburg| Tirol| Vorarlberg"),
         Straße = Straße %>% str_replace("-", "*")) %>% 
  mutate(Straße = case_when(Straße %>% str_detect("Widdmann") ~ "Weiz*Dr.-Karl-Widdmann-Straße 40",
                            Straße %>% str_detect("Renner") ~ "Liezen*Dr.-Karl-Renner-Ring 40",
                            Straße %>% str_detect("Theresianum, Kalvarienbergplatz") ~ "Eisenstadt*Kalvarienbergplatz 8",
                            Straße %>% str_detect("Langenloiser") ~ "Krems*Langenloiser Straße 22",
                            Straße %>% str_detect("Niederösterreich-Klosterstraße") ~ "Schlierbach*Klosterstraße 10",
                            .default = Straße)) %>% 
    separate(Straße, sep = "\\*", into = c("Gemeinde","Straße")) %>% 
  select(Art, BL, Gemeinde, Straße) %>% 
  arrange(Gemeinde, Straße)
schulen %>% print(n=200)

gesamtliste1 <- 
  gesamtliste %>% 
  mutate(Straße = Adresse %>% str_remove(", \\d{4}.+"),
         Gemeinde = Adresse %>% str_remove(".+, \\d{4} ")) %>% 
  arrange(Gemeinde, Straße) #%>%
# gesamtliste1 %>% 
#   write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/Gesamtliste-Schulen.xlsx")


## Erfolgreiche Anbindung ----  
inner_join(schulen, gesamtliste1,
           # by=c("Gemeinde","Straße"),
           by=c("Straße"),
           relationship = "many-to-many"
           ) %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/joint_schulen1.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/joint_schulen1.xlsx")
  
merge <- 
  inner_join(schulen, gesamtliste1,
           # by=c("Gemeinde","Straße"),
           by=c("Straße"),
           relationship = "many-to-many"
) %>% 
  mutate(strng = Art %>% str_sub(1,4) %>% str_to_lower(),
         strng = case_when(Art %>% str_detect("päda") ~ "pädag", 
                           Art %>% str_detect("Kaufm") ~ "handel",
                           Art %>% str_detect("Sozial") ~ "wirt",
                           .default = strng),
         Schule = Schule %>% str_to_lower()) %>% 
  mutate(abgleich = if_else(Schule %>% str_detect(strng), T, F)) %>% 
  # select(Art, Schule, strng, abgleich, Straße) %>%
  distinct() 

### Kontrolle, ob bei abgleich == F auch wirklich fehlerhaft
merge %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/joint_schulen.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/joint_schulen.xlsx")

### bereinigen
merge1 <- 
  merge %>% 
  mutate(abgleich = case_when(Straße %>% str_detect("Keramikstraße") ~ T,
                              Schule %>% str_detect("modeschule hetzendorf") ~ T,
                              Schule %>% str_detect("schulen des bfi wien") ~ T,
                              Straße %>% str_detect("Michelbeuerngasse 12") ~ T,
                              Straße %>% str_detect("Rosensteingasse 79") ~ T,
                              Straße %>% str_detect("Ungargasse 69") ~ T,
                              Straße %>% str_detect("Schneeberggasse 26") & Schule %>% str_detect("Mode") ~ T,
                              .default = abgleich)) %>% 
  filter(abgleich)

merge1 <- 
  merge1 %>% 
  select(-Gemeinde.y) %>% 
  rename(Gemeinde = Gemeinde.x)



## Fehlende Anbindung ----
anti <- 
  anti_join(schulen, gesamtliste1,
          # by=c("Gemeinde","Straße"),
          by=c("Straße")
          ) %>% 
  arrange(Gemeinde, Straße) %>% 
  distinct() 

### Bereinigen
anti %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/fehlendeanbindung_schulen.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/fehlendeanbindung_schulen.xlsx")

gesamtliste1 %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/gesamtliste_schulen.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/gesamtliste_schulen.xlsx")
  
anti1 <- read.xlsx("../rueckmeldungen/rueckmeldungen_schulen/fehlendeanbindung_schulen1.xlsx") %>% 
  as_tibble()


## Zusammenführen ----

zus <- rbind(merge1 %>% select(all_of(names(anti1))),
      anti1) %>% 
  mutate(id = row_number())
  
zus1 <- 
  schulen %>% 
  left_join(zus, by = c("Art","BL","Gemeinde","Straße")) %>% 
  arrange(id) 
  
zus2 <- 
  zus1 %>% 
  mutate(Email = case_when(Art %>% str_detect("Techn") & Straße %>% str_detect("Schneeberggasse 26") ~ "hlm.wr-neustadt@noeschule.at",
                           Art %>% str_detect("Techn") & Straße %>% str_detect("Kissingen") ~ "direktion@htleisenstadt.at",
                           Art %>% str_detect("Kaufm") & Straße %>% str_detect("Langenloiser") ~ "office@hakkrems.ac.at",
                           .default = Email)) 

zus3 <- 
  zus2 %>% 
  mutate(Art.k = Art %>% str_replace("Wirtschaft.+Schule","Wirt") %>% 
           str_replace("Touri.+Schule","Tour") %>% 
           str_replace("Tourismus","Tour") %>% 
           str_replace("Sozial.+Schule","Sozial") %>% 
           str_replace("Techn.+Schule","Techn") %>% 
           str_replace("Bildungs.+pädagogisch","Pädag") %>% 
           str_replace("Kaufmännisch.+Schule","Kaufm"),
         BL.k = BL %>% str_replace("Wien(?= )","W") %>% 
           str_replace("Burgenland","B") %>% 
           str_replace("Kärnten","K") %>% 
           str_replace("Niederösterreich","NÖ") %>% 
           str_replace("Oberösterreich","OÖ") %>% 
           str_replace("Steiermark","Stk") %>% 
           str_replace("Salzburg","Sbg") %>% 
           str_replace("Tirol","T") %>% 
           str_replace("Vorarlberg","V") %>% 
           str_replace("Wien","W"),
         kenn1 = paste0(Art.k, "-", BL.k))

zus4 <-
  zus3 %>% 
  mutate(across(c(Gemeinde, Straße), ~ .x %>% str_replace_all(" ", "_"), .names = "{.col}.u"),
         anschr = paste0(Gemeinde.u, "-", Straße.u) %>% 
           str_replace_all("ö", "oe") %>% 
           str_replace_all("ä", "ae") %>% 
           str_replace_all("ü", "ue") %>% 
           str_replace_all("ß", "ss"),
         kenn1 = kenn1 %>% 
           str_to_lower() %>% 
           str_replace_all("ö", "oe") %>% 
           str_replace_all("ä", "ae") %>% 
           str_replace_all("ü", "ue") %>% 
           str_replace_all("ß", "ss")) %>% 
  arrange(kenn1, anschr)
  # separate_wider_delim(Art, names = c("Art","BL"), delim = "_ ") %>% 
  # mutate(Adresse = str_replace(Adresse, "-", "**")) %>% 
  # separate_wider_delim(Adresse, names = c("Gemeinde", "Straße"), delim = "**") %>% 
  # mutate(across(everything(), ~ .x %>% str_replace_all(" ","_")),
  #        Anschrift = paste(Art, BL, Gemeinde, Straße, sep = "-")) 
zus4 %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/gesamtliste_schulen_zus.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/gesamtliste_schulen_zus.xlsx")  


## Abgleich mit vorhandenen Dokumenten ----
words <- dir_ls("../rueckmeldungen/rueckmeldungen_schulen/word") %>% 
  as.vector()

words1 <-
  words %>% 
  str_remove("../rueckmeldungen/rueckmeldungen_schulen/word/") %>% 
  as_tibble() %>% 
  mutate(kenn1 = value %>% 
           str_to_lower() %>% 
           str_extract("^\\w+-\\w+"),
         anschr = value %>% str_remove("^\\w+-\\w+-") %>% 
           str_remove("\\.docx"),
         anschr = anschr %>% str_replace_all("ö", "oe") %>% 
           str_replace_all("ä", "ae") %>% 
           str_replace_all("ü", "ue") %>% 
           str_replace_all("ß", "ss"),
         kenn1 = kenn1 %>% 
           str_to_lower() %>% 
           str_replace_all("ö", "oe") %>% 
           str_replace_all("ä", "ae") %>% 
           str_replace_all("ü", "ue") %>% 
           str_replace_all("ß", "ss"),
         reihenfolge.datei = 1:nrow(.),
         anschr = case_when(anschr %>% str_detect("Widdmann") ~ "Weiz-Dr.-Karl-Widdmann-Straße_40",
                            anschr %>% str_detect("Renner") ~ "Liezen-Dr.-Karl-Renner-Ring_40",
                            anschr %>% str_detect("Theresianum,_Kalvarienbergplatz") ~ "Eisenstadt-Kalvarienbergplatz_8",
                            anschr %>% str_detect("Langenloiser") ~ "Krems-Langenloiser_Straße_22",
                            anschr %>% str_detect("Niederösterreich-Klosterstraße") ~ "Schlierbach-Klosterstraße_10",
                            .default = anschr)
  )

trema <- words1[4,"anschr"] %>% str_sub(17,17) # Word bildet Umlaute als Buchstabe mit einem Trema

words2 <- 
  words1 %>% 
  mutate(anschr = str_replace_all(anschr, trema, "e") %>% 
           str_replace_all("ß","ss"),
         kenn1 = str_replace_all(kenn1, trema, "e") %>% 
           str_replace_all("ß","ss"))

# schulnamen.df$Anschrift %>% 
#   sort() %>% 
#   as.data.frame() %>% 
#   as_tibble() %>% 
#   rename(anschr = 1) %>% 
#   mutate(kenn1 = anschr %>% str_extract("^\\w+-\\w+"),
#          anschr = anschr %>% str_remove("^\\w+-\\w+-"),
#          reihenfolge.schulnamen = 1:nrow(.)) %>% 
#   full_join(words1)
# 
# %>% 
#   str_remove_all(" $") %>% 
#   str_remove_all("^ ") %>% 
#   str_replace_all("ö", "oe") %>% 
#   str_replace_all("ä", "ae") %>% 
#   str_replace_all("ü", "ue") %>% 
#   str_replace_all("ß", "ss")

abgl1 <-
  zus4 %>%
  left_join(words2, by = c("anschr","kenn1"),
            relationship = "many-to-many") %>% 
  group_by(anschr) %>% 
  mutate(n = n()) %>% 
  arrange(anschr) %>% 
  filter(if_any(reihenfolge.datei, ~ !.x %>% is.na())) 

# abgl2 <-
abgl1 %>% 
  select(-id) %>% 
  filter(n > 1) %>% 
  distinct() %>% 
  View()
  

abgl2 <- 
  abgl1 %>% 
  mutate(filter = case_when(kenn1 == "wirt-b" & anschr %>% str_detect("Neusiedl_am_See-Bundesschulstrasse_4") & Email == "office@akwi.at" ~ F,
                            kenn1 == "wirt-ooe" & Email %>% str_detect("hblawb-wolfsb@lsr-ktn.gv.at") ~ F,
                            kenn1 == "kaufm-w" & Email %>% str_detect("office@bbs-rohrbach.at") ~ F,
                            kenn1 == "kaufm-k" & Email %>% str_detect("office@hakried.at") ~ F,
                            .default = T)) %>% 
  filter(filter) %>% 
  arrange(reihenfolge.datei)
  


## Bei der/den folgende/n Schule/n kann nicht genau zugeordnet werden, welche Schule es bei mehreren genau betrifft. Die Berichte werden gänzlich entfernt
abgl3 <- 
  abgl2 %>% 
  mutate(filter = if_else(kenn1 == "kaufm-ooe" & anschr %>% str_detect("Stelzham"), F, T)) %>% 
  filter(filter == T)
abgl3 %>% 
  write.xlsx("../rueckmeldungen/rueckmeldungen_schulen/Adresse_Datei_zugeordnet.xlsx")
system("open ../rueckmeldungen/rueckmeldungen_schulen/Adresse_Datei_zugeordnet.xlsx")

is_file("../rueckmeldungen/rueckmeldungen_schulen/word/Kaufm-OOÌˆ-Wels-Stelzhamerstraße_20.docx")
file_delete("../rueckmeldungen/rueckmeldungen_schulen/word/Kaufm-OOÌˆ-Wels-Stelzhamerstraße_20.docx")
is_file("../rueckmeldungen/rueckmeldungen_schulen/pdfs/Kaufm-OÖ-Wels-Stelzhamerstraße_20.pdf")
file_delete("../rueckmeldungen/rueckmeldungen_schulen/pdfs/Kaufm-OÖ-Wels-Stelzhamerstraße_20.pdf")


# Serienmails ----
serienpath <- str_remove(getwd(), "auswertung")
serienfiles <- 
  dir_ls(paste0(serienpath,"rueckmeldungen/rueckmeldungen_schulen/pdfs"),
       all = T) %>% 
  as.vector()

serienmail <- abgl3
serienmail["serienfiles"] <- serienfiles
serienmail["Betreff"] <- "Ergebnisse zur Studie Erfahrungen mit Pflichtpraktika in berufsbildenden Schulen"

serienmail <- 
  serienmail %>% 
  ungroup() %>% 
  select(anschr, Email, Betreff, serienfiles)

write.xlsx(serienmail, "../rueckmeldungen/rueckmeldungen_schulen/serienmail.xlsx")

system("open ../rueckmeldungen/rueckmeldungen_schulen/serienmail.xlsx")
# alt <- read.xlsx("../rueckmeldungen/pp_schulen_email.xlsx") %>% 
#   as_tibble()
# schulnamen.df <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulnamen.df.RData")
# 
# alt2 <-
# alt %>% 
#   na.omit %>% 
#   rename(Adresse = 1,
#          email = 2) %>% 
#   mutate(Adresse = str_replace(Adresse, " - ", "*"),) %>% 
#   separate_wider_delim(Adresse, delim = "*", names = c("Art", "Gemeinde")) %>% 
#   mutate(Art = Art %>% 
#            str_replace("Wirtsch.+","Wirt") %>% 
#            str_replace("Touri.+","Tour") %>% 
#            str_replace("Sozial.+","Sozial") %>% 
#            str_replace("Techn.+","Techn") %>% 
#            str_replace("Bildungs.+","Pädag") %>% 
#            str_replace("Kaufmänn.+","Kaufm"),
#          Straße2 = Gemeinde %>% str_extract("(?<= - ).+"), .after = Gemeinde,
#          Gemeinde = Gemeinde %>% 
#            str_remove(" - .+") %>% 
#            str_remove("\\d{4} "))
# 
# schulnamen.df %>% 
#   mutate(Straße2 = Straße %>% str_remove("_\\d+.*") %>% 
#            str_replace("_"," "), .after = Straße,
#          Gemeinde = str_replace_all(Gemeinde, "_"," ")) %>% 
#   # filter(Gemeinde == "Wien")
#   full_join(alt2, by = c("Art","Gemeinde","Straße2")) %>% 
#   View()