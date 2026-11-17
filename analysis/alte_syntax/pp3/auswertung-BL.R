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



# Einlesen ----
pp_tb <- readRDS("pp_tb.RData")
# lbl.var.df <- readRDS("Variablenabgleich_alt_neu.RData")

# vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/")
# # source(paste0(vorlagen.path,"functions_tabellenband.R"), encoding = "latin1")
# source(paste0(vorlagen.path,"R-Scripts/functions_praktisch.R"), encoding = "latin1")

getwd() %>% str_remove("(?<=Georgkessler).+")

# Tabellen erstellen ----

## Daten aufbereiten ----

pp_tb <- cbind(pp_tb,
               pp_tb %>%
                 select(ausbildungsfeld, matches("q05")) %>% 
                 rowwise %>% 
                 mutate(Schule = paste0(c_across(everything()), collapse = " ") %>% 
                          str_remove_all(" NA")) %>% 
                 select(Schule)) %>% 
  as_tibble

pp_tb <- pp_tb %>% 
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

pp_tb$g22_rec <- set_label(pp_tb$g22_rec,
                           "Wie haben Sie den Praktikumsbetrieb gefunden?")


## Auswahl der Variablen ----

cb <- fnc.codebook(pp_tb)
schul.vars <- c("q01_rec","q16","q18_anz_bew_kat2","g20_1","g20_2","g22_rec",
                "q23_1","q23_2","q23_3","q23_4","q23_5","q23_6","q23_7","q23_8","q23_9","q23_10","q23_11","q23_12","q23_13",
                "q24","q26","q30","q32","q33_2","q34","q36","q38_einkommen_gesamt_kat","q40_kosten_gesamt_kat",
                "q43","q44_4","q44_5","q45_1","q45_2","q45_7","q45_8","q46_SQ001","q46_SQ002","q46_SQ003","q49",
                "q53_1","q53_2","q53_3","q53_4","q55","q27","cluster_qualitaet_3")

lbl.schul.vars <-
  cb %>%
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



## Tabellen erstellen ----
bundesl <- pp_tb$Bundesland %>% unique %>% sort
j=1
i=1
l1 <- list()
for (j in 1:length(bundesl)) {
  
  bl <- bundesl %>% .[[j]] # selektion des Bundeslandes
  df <- pp_tb %>% 
    mutate(bl.filter = if_else(Bundesland == bl, bl, "Andere Bundesländer"))
  ### letzter Stand
  # Gesamtzeile: Totale ohne Gewichtung
  gesamt <- 
    bind_cols(Fragestellung = "Gesamt",
              Kategorie = "-",
              df %>% 
                count(bl.filter, ausbildungsfeld_rec, name = "Anzahl") %>% 
                na.omit %>%
                mutate(Kat = paste0(bl.filter, " - ", ausbildungsfeld_rec)) %>% 
                mutate(bl.filter = factor(bl.filter) %>% fct_rev()) %>% 
                arrange(ausbildungsfeld_rec, bl.filter) %>%
                select(-c(ausbildungsfeld_rec, bl.filter)) %>%
                pivot_wider(names_from = Kat, values_from = Anzahl) %>% 
                mutate(Gesamt = rowSums(.[-c(1,ncol(.))])) %>% 
                mutate(across(2:ncol(.), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ","))) # Spalten formatieren
              
    )
  
  # Tabelle 
  l1[[j]] <- list()
  for (i in 1:length(schul.vars)) {
    
    gesamt.p <- # hier wird die Gesamtspalte erzeugt, die anschließend an l1[[i]][[i]] angehängt wird
      df %>%
      count(across(c(schul.vars[i])), wt = gewicht) %>%
      na.omit %>% 
      mutate(Gesamt = format(round(n/sum(n)*100, 0) %>% paste0(" %"), big.mark = ".", decimal.mark = ",")) %>% 
      select(-n) %>% 
      bind_rows(list(tibble(!!sym(schul.vars[i]) := "Gesamt",
                            Gesamt = "100 %"),
                     tibble(!!sym(schul.vars[i]) := "Anazhl",
                            Gesamt = df %>% nrow %>% format( big.mark = ".", decimal.mark = ","))
      )
      ) %>% 
      select(Gesamt) %>% 
      mutate(Gesamt = if_else(Gesamt %>% str_detect("NA|NaN"), "-", Gesamt))
    
    
    anzahl.ungewichtet <- # Anzahl ungewichteter Ns
      df %>%
      count(across(c(bl.filter, ausbildungsfeld_rec, schul.vars[i])), name = "Anzahl") %>%
      na.omit %>%
      mutate(bl.filter = factor(bl.filter) %>% fct_rev(),
             Kat = paste0(bl.filter, " - ", ausbildungsfeld_rec)) %>%
      group_by(bl.filter, ausbildungsfeld_rec) %>%
      mutate(Anzahl = sum(Anzahl),
             Gesamt = 100) %>%
      mutate(across(where(is.numeric), ~ format(round(.x, 0), big.mark = ".", decimal.mark = ",")),
             across(c(Gesamt), ~ .x %>% paste0(" %"))) %>%
      ungroup %>%
      select(-c(schul.vars[i])) %>%
      distinct %>%
      pivot_longer(-c(Kat, ausbildungsfeld_rec, bl.filter), names_to = "Kategorie") %>%
      mutate(Kategorie = Kategorie %>% factor %>% fct_rev) %>%
      arrange(ausbildungsfeld_rec, bl.filter, Kategorie) %>% # Sortieren, um später bei Umformungen die Ausbildungsrichtungen der Vergleichsgruppen nebeneinander zu haben
      select(-c(ausbildungsfeld_rec, bl.filter)) %>%
      pivot_wider(names_from = Kat, values_from = value) %>%
      mutate(Fragestellung = lbl.schul.vars$Frage.neu[i], .before = 1)
    
    
    l1[[j]][[i]] <- 
      df %>% 
      count(across(c(bl.filter, ausbildungsfeld_rec, schul.vars[i])), wt = gewicht) %>% # gewichtete Ns berechnen
      group_by(bl.filter, ausbildungsfeld_rec) %>%
      na.omit %>% 
      mutate(p = n/sum(n)*100) %>% 
      ungroup %>%
      mutate(bl.filter = factor(bl.filter) %>% fct_rev(),
             Kat = paste0(bl.filter, " - ", ausbildungsfeld_rec)) %>% 
      arrange(ausbildungsfeld_rec, bl.filter) %>% # Sortieren, um später bei Umformungen die Ausbildungsrichtungen der Vergleichsgruppen nebeneinander zu haben
      select(Kat, schul.vars[i], p) %>%
      pivot_wider(names_from = Kat, values_from = p) %>% 
      mutate(Fragestellung = lbl.schul.vars$Frage.neu[i], .before = 1) %>% # Fragestellung einfügen
      mutate(across(3:ncol(.), ~ format(round(.x, 0) %>% paste0(" %"), big.mark = ".", decimal.mark = ","))) %>% 
      rename(Kategorie = 2) %>% 
      bind_rows(anzahl.ungewichtet) %>% # ungewichtete Ns hinzufügen
      bind_cols(gesamt.p) %>% # GEsamtspalte hinzufügen
      mutate_all(~ if_else(.x %>% str_detect("NA|NaN"), "-", .x))
    
  }
  
  l1[[j]] <- l1[[j]] %>%
    list_rbind()
  
}

l1[[1]] %>%
  View

saveRDS(l1, file = "../rueckmeldungen/rueckmeldungen_BL/tb_bl.RData")
# l1 <- readRDS(file = "../rueckmeldungen/rueckmeldungen_BL/tb_bl.RData")

# l1[1:2]


# PRüfen, ob Schultypen pro Bundesland mind. 2x vorkommen
library(openxlsx)

sl <- 
  read.xlsx("../frabo/ministerium_schulliste.xlsx", sheet = "oeibf_bearbeitet") %>% 
  as_tibble()

names(l1) <- bundesl  

sl %>% 
  select(Bundesland, Schultyp_rec, Ausbildungsfeld_rec, Schulformbezeichnung_rec) %>% 
  mutate(Ausbildungsfeld_rec = case_when(Schulformbezeichnung_rec %>% str_detect("Tourismus") ~ "Tourismus",
                                         Schulformbezeichnung_rec %>% str_detect("Sozialberuf") ~ "Sozial",
                                         .default = Ausbildungsfeld_rec)) %>% 
  filter(!Ausbildungsfeld_rec %>% str_detect("forstwirt")) %>% 
  group_by(Bundesland, Schultyp_rec) %>%
  # group_by(Bundesland) %>% 
  count(Ausbildungsfeld_rec) %>% 
  filter(n == 1)


l1$Kärnten <-
  l1$Kärnten %>% 
  select(-matches("Kärnten - Touris")) 

l1$Steiermark <- l1$Steiermark %>% 
  select(-matches("Steiermark - Touris"))

l1$Oberösterreich <- l1$Oberösterreich %>% 
  select(-matches("Oberösterreich - Sozial"))

l1$Salzburg <- l1$Salzburg %>% 
  select(-matches("Salzburg - Sozial"))

l1$Tirol <- l1$Tirol %>%
  select(-matches("Tirol - Sozial"))

l1$Vorarlberg <- l1$Vorarlberg %>% 
  select(-matches("Vorarlberg - Sozial"))


# Flextables ----
data <- l1[[9]]
bl <- bundesl[9]

fnc.flext <- function(data, bl) { # bl wird gebraucht, da nicht alle Bundesländer alle Schultypen haben, was die Anzahl der Spalten verändert --> bgs
  
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
  # colums with background
  bgs <- which(names(data) %>% str_detect(bl))
  # create flextable
  data %>% 
    flextable %>% 
    merge_v(j = 1, part = "body") %>% 
    rotate(i = 1, j = 3:dims[2], rotation = "btlr", part = "header") %>% 
    valign(j = 1, valign = "top", part = "body") %>% 
    valign(valign = "center", part = "header") %>% 
    valign(j = 1:2, valign = "bottom", part = "header") %>%
    align(i = 1, j = 2, align = "left", part = "header") %>% 
    align( j = 3:dims[2], align = "center", part = "header") %>%
    align(j = 3:dims[2], align = "right", part = "body") %>% 
    bold( part = "header") %>%
    fontsize(size = 7, part = "all") %>% 
    font(fontname = "Calibri", part = "all") %>% 
    colformat_num(big.mark = ".", decimal.mark = ",") %>% 
    width(j = 1, width = c(3.5), unit = "cm") %>%
    width(j = 2, width = c(3.5), unit = "cm") %>%
    width(j = 3:dims[2], width = (18-7)/(dims[2]-2), unit = "cm") %>% # 17 = beschreibbare Seitenbreite; 7 = 1. + 2. Spaltenbreite
    hrule(rule = "atleast", part = "all") %>% 
    height(height = 4, unit = "cm", part = "header") %>%
    height(height = 0.53, unit = "cm", part = "body") %>% 
    bg(j = bgs, bg="lightgrey", part = "body") %>% 
    bg(j = bgs, bg="lightgrey", part = "header") %>% 
    bg(j = dims[2], bg="#F8F8F8", part = "body") %>% 
    border_outer(border = fp_border_default()) %>% 
    border_inner(border = fp_border_default(), part = "all") %>%
    paginate(group = "Fragestellung", group_def = "rle") %>% 
    padding(padding = 1, part = "all") %>% 
    padding(padding.right = 2.5, part = "all") %>% 
    hline(i = borders, border = fp_border(width = 1.8), part = "body") %>% 
    hline(border = fp_border(width = 1.8), part = "header")
  
}

ft.list.bl <- map2(l1, bundesl, fnc.flext)
saveRDS(ft.list.bl, file = "../rueckmeldungen/rueckmeldungen_BL/ft_list.RData")
# ft.list.bl <- readRDS(file = "../rueckmeldungen/rueckmeldungen_BL/ft_list.RData")


# Word erstellen ----
bold_face <- shortcuts$fp_bold()

# doc.list <- list()
for (k in 1:length(ft.list.bl)) {
  # for (k in 1:1) {
  
  read_docx(path = "Tabellenband_rueckmeldungen_template.docx") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Erfahrungen mit Pflichtpraktika in den berufsbildenden Schulen", style = "Title") %>%
    body_add_par(value = "-", style = "Subtitle") %>%
    body_add_par(value = "Bundeslandspezifische Rückmeldungen für", style = "Subtitle") %>%
    body_add_par(value = bundesl[k], style = "Subtitle") %>% # Bundesland einfügen
    body_add_par(value = "Sehr geehrte Bildungsdirektion,", style = "Fließtext") %>%
    body_add_par(value = "das Österreichische Institut für Berufsbildungsforschung (öibf) untersuchte im Auftrag der Arbeiterkammer Wien und des Bundesministeriums für Bildung, Wissenschaft und Forschung (GZ 2023-0.278.280) die Umsetzung von Pflichtpraktika bei berufsbildenden Schulen in Österreich. Der komplette Endbericht liegt nun vor und ist frei abrufbar unter www.oeibf.at.", style = "Fließtext") %>%
    body_add_par(value = "Wie im Oktober 2023 zugesagt, dürfen wir Ihnen nun als speziellen Dank für Ihre Unterstützung eine bundesländerspezifische Auswertung übermitteln, in der Sie die Ergebnisse Ihres Bundeslandes mit jenen der anderen Bundesländer vergleichen können. Die Ergebnisse dienen ausschließlich zu Ihrer internen Verwendung.", style = "Fließtext") %>%
    body_add_par(value = "", style = "Fließtext") %>%
    # body_add_par(value = "", style = "Fließtext") %>%
    body_add_fpar(value = fpar(ftext("Hinweise zur Interpretation:", prop = bold_face)), style = "Fließtext") %>%
    body_add_par(value = 'In den Vergleichsgruppen finden sich Schulen für die groben Ausbildungsrichtungen "kaufmännisch", "technisch-gewerblich", "wirtschaftsberufliche", "touristisch", „pädagogisch“ und „sozialberuflich“. Für genauere Informationen zur Stichprobe dürfen wir auf den Endbericht verweisen, der auf unserer Website http://www.oeibf.at frei abrufbar ist.', style = "List Bullet") %>%
    body_add_par(value = "Bei den schulspezifischen Auswertungen können die jeweiligen Stichproben sehr klein sein. Daraus ergeben sich erhebliche statistische Schwankungsbreiten. Die Ergebnisse können aus methodischer Sicht daher lediglich als Orientierungshilfe gesehen werden, um eventuelle Tendenzen abzuleiten.", style = "List Bullet") %>%
    body_add_par(value = 'Einige Fragen wurden als Filterfragen konzipiert. Dies spiegelt sich in einer hohen Anzahl von fehlenden Werten wider. Diese wurden mit "-" gekennzeichnet. Ein Prozentwert von "0 %" verweist auf einen gerundeten Wert kleiner als 0,5 %.', style = "List Bullet") %>%  
    body_add_par(value = 'In manchen Bundesländern nahmen Schüler:innen aus Schulen bestimmter Ausbildungsrichtungen nicht an der Umfrage teil. Die entsprechenden Spalten werden nicht gezeigt.', style = "List Bullet") %>%  
    body_add_par(value = 'Um den Schulen Anonymität zu versichern, wurde nur nach jenen Ausbildungsrichtungen ausgewertet, für die mindestens zwei Schulen pro Bundesland ausbilden. Die Durchschnittswerte der übrigen Bundesländer für diese Richtung werden dennoch dargestellt.', style = "List Bullet") %>%  
    # body_add_par(value = "", style = "Fließtext") %>%
    body_add_par(value = "", style = "Fließtext") %>%
    body_add_fpar(value = fpar("Herzlichen Dank für Ihre Unterstützung, auch im Namen des Projektteams!", 
                               run_linebreak(), 
                               ftext("Dr. Norbert Lachmayr"),
                               run_linebreak(), 
                               ftext("öibf - Margaretenstraße 166/2. Stock, 1050 Wien; ZVR-Nr.: 718743404; www.oeibf.at")),
                  style = "Fließtext_li") %>%
    # body_add_par(value = "Herzlichen Dank für Ihre Unterstützung, auch im Namen des Projektteams!", style = "Fließtext") %>%
    # body_add_par(value = "Dr. Norbert Lachmayr", style = "Fließtext") %>%
    # body_add_par(value = "öibf - Margaretenstraße 166/2. Stock, 1050 Wien; ZVR-Nr.: 718743404; www.oeibf.at", style = "Fließtext") %>% 
    body_add_flextable(value = ft.list.bl[[k]]) %>%
    print(target = paste0("../rueckmeldungen/rueckmeldungen_BL/Bildungsdirektion_", bundesl[k], ".docx"))
  
}  


### Create pdf
## alle docx in pdf24-creator einspeisen und konvertieren lassen
# docfile <- paste0("../rueckmeldungen/rueckmeldungen_BL/Bildungsdirektion_", bundesl[k], ".docx")
# system(paste("X:/path_to_libreoffice/program/soffice.exe --headless --convert-to pdf", docfile), intern = TRUE)
# 
# system("open ../rueckmeldungen/rueckmeldungen_BL/Bildungsdirektion_Wien.docx")
# 
# k=1
# pandoc <- "C:\\Programme\\pandoc.exe"
# input <- paste0("C:\\Users\\GeorgKessler\\OneDrive - OEIBF\\data\\1_Projekte\\23_06_AK_Pflichtpraktikum3\\rueckmeldungen\\rueckmeldungen_BL\\Bildungsdirektion_", bundesl[k], ".docx")
# output <- paste0("C:\\Users\\GeorgKessler\\OneDrive - OEIBF\\data\\1_Projekte\\23_06_AK_Pflichtpraktikum3\\rueckmeldungen\\rueckmeldungen_BL\\Bildungsdirektion_", bundesl[k], ".pdf")
# cmd <- sprintf('"%s" "%s" -o "%s"', pandoc, input, output)
# shell(cmd)
# 
# system(paste("C:/Programme/LibreOffice/program/soffice.exe --headless --convert-to pdf", docfile), intern = TRUE)
