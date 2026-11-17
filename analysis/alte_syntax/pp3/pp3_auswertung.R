# Vorbereiten ----
## Libraries laden ----
library(tidyverse)
library(haven)
library(psych)
library(sjlabelled)
library(sjmisc)
library(lubridate)
library(openxlsx)
library(factoextra)
library(ggplot2)

conflicted::conflicts_prefer(dplyr::filter(),
                             dplyr::lag(),
                             dplyr::select(),
                             janitor::chisq.test(),
                             dplyr::select(),
                             lme4::lmer(),
                             sjlabelled::as_factor())


## Functions importieren ----
# ein anderer Pfad für Testdaten --> Chunk: test-laden
test <- T 
# Dispense-Analyse nicht weiterverfolgt von Martin
dispens <- F

vorlagen.path <- paste0(getwd() %>% str_remove("(?<=OEIBF\\/).+"), "OEIBF - Administration/6_Vorlagen und Standards/")
source(paste0(vorlagen.path, "/6_10_R_Vorlagen/functions_praktisch.R"), encoding = "utf-8")
source(paste0(vorlagen.path, "/6_10_R_Vorlagen/functions_tabellenband.R"), encoding = "utf-8")
# source(paste0(vorlagen.path,"R-Scripts/functions_tabellenband.R"), encoding = "latin1")
# source(paste0(vorlagen.path,"R-Scripts/functions_praktisch.R"), encoding = "latin1")

## Einlesen ----
### Schullisten ----
# source("02_Datenaufbereitung.R")
adress.liste <- read.xlsx("../frabo/ministerium_schulliste.xlsx", sheet = "oeibf_bearbeitet") %>%
  as_tibble()
ad.l <- read.xlsx("../frabo/ministerium_schulliste.xlsx", sheet = "BMHS - Version 1") %>%
  as_tibble()
ad.l %>% 
  rename(bmhs = 'M-H-S') %>% 
  filter(!Schulformbezeichnung %>% str_detect("Land|Forst|Mode|Kunst|Gestaltung")) %>% 
  filter(!Ausbildungsfeld %>% str_detect("Land-")) %>% 
  mutate(Ausbildungsfeld_rec = case_when(Schulformbezeichnung %>% str_detect("Sozialberuf") ~ "Schule für Sozialberuf",
                                         Schulformbezeichnung %>% str_detect("Tourismus") ~ "Schule für Tourismus",
                                         .default = Ausbildungsfeld),
         Adresse_rec = paste(Bundesland, Straße, sep = " - ")) %>% 
  count(Bundesland, Ausbildungsfeld_rec) 
  # count(bmhs, Ausbildungsfeld_rec, Bundesland, Straße) %>% 
  # View
  # filter(Schulformbezeichnung %>% str_detect("Sozialberuf")) #%>% 
  # count(Ausbildungsfeld, Ausbildungsfeld_rec)
  # count(Ausbildungsfeld)
  # filter(Schulformbezeichnung %>% str_detect("techn")) %>% 
  
### humanberufliche Schulen: Schulen für Wirtschaft, Tourismus, Mode, Kunst sowie Sozialberufe
  # reframe(Schulkennzahl,
  #         )



### PP Rohdaten ----
pp_origin <- readRDS("pp3.RData") %>% 
  as_factor()
fb <- read.csv(paste0("../frabo/limesurvey_survey_", 994263, ".txt"), sep="\t") %>%
  as_tibble()
shell.exec(paste0("../frabo/limesurvey_survey_", 994263, ".txt") %>% normalizePath())
cb <- fnc.codebook(pp_origin, fb)

cb


### Itemliste ----
fb.namen <- fnc.fb.namen(fb)
cb <- left_join(cb, fb.namen,
                by = "strng.old.name")

itemlist <-
cb %>% 
  filter(paradata == F) %>% 
  select(name, strng, label, alpha) %>% 
  filter(!name %>% str_detect("Bundesland|schultyp|_rec|itemnonr|ausbildungsfeld|gewicht|Stadt|Adresse")) %>% 
  mutate(Gruppe = NA, .after = 1) %>%
  tidyr::fill(alpha) %>% 
  mutate(rid = match(strng, unique(strng)), .by = alpha) %>% 
  mutate(alpha = paste(alpha, rid, sep = ""),
         alpha = if_else(alpha %>% str_detect("NA"), NA, alpha)) %>% 
  relocate(rid, strng, .after = last_col()) %>%
  # mutate(strng = case_when(strng %>% str_detect("q02") ~ "q02",
  #                          strng %>% str_detect("q03") ~ "q03",
  #                          strng %>% str_detect("q05") ~ "q05",
  #                          .default = strng)) %>% 
  mutate(un.id = match(alpha, unique(alpha))) %>%
  select(-rid, -strng) %>% 
  rename_all(~ c("Fragencode", "Gruppe", "Fragebeschriftung", "FB_pp3", "kum. Frageanzahl"))
  # print(n=Inf)
  
#### Zähler für Itemanzahl in Excel ----
# t <- itemlist %>% 
#   mutate(across(everything(), ~.x == NA),
#          across(everything(), ~.x %>% 
#                   as.character())) %>% 
#   slice(1) 
# l <- LETTERS[which(names(t) %>% str_detect("FB_"))]
# 
# replace <-
# paste0("=ANZAHL2(",l,"2:",l,nrow(itemlist),")") %>% 
#   as.data.frame() %>% 
#   t() # transpose
# t[1, names(t) %>% str_detect("FB_")] <- replace
# t[1,1] <- "Anzahl Fragen pro Welle"

wb <- createWorkbook()
addWorksheet(wb, "Itemliste_PP")
# writeDataTable(wb, "Itemliste_PP", bind_rows(t, itemlist), tableStyle = "TableStyleMedium2")#,startCol = which(colnames(df)=="URL"), startRow = 2)
writeDataTable(wb, "Itemliste_PP", itemlist, tableStyle = "TableStyleMedium2")
saveWorkbook(wb, "dokumentation_items_pp1_pp3.xlsx", overwrite = TRUE)
system("open dokumentation_items_pp1_pp3.xlsx")


## alte PP ----
# load("pp2.RData")
# pp.alt_dispens <- pp2_dispens
# pp.alt <- pp2
# rm(pp2)

### Vergleich zwischen pp2 und pp3 in Variablen herstellen
# x <- which(names(pp_dispens) %in% c("q08","q16"))
# y <- get_label(pp_dispens[x[1]:x[2]])
# pp3.dispense <- tibble(Variable=names(y),
#        Label = y) %>%
#   filter(Label %>% str_detect("Nachweise|keine .+ gefunden|unterlagen geschickt")) %>%
#   mutate(key = Label %>% str_extract("(?<=\\[).+(?=\\])"))
# 
# pp2.dispense <- readRDS(file = "labelsXXX.RData")
# pp2.dispense <- pp2.dispense %>%
#   mutate(key = Label %>% str_extract("(?<=\\[).+(?=\\])")) %>%
#   filter(Variable %>% str_detect("q14_1|q14_2|q8", negate = T))
# 
# a <- pp3.dispense %>%
#   full_join(pp2.dispense, by=c("Label"))
# 
# a[which(a$Variable.x %>% str_detect("q15_")),"Variable.y"] <- c("q10_1","q10_2")
# a <- a[-(19:20),]
# a <- a[,-5]


# Datenaufbereitung ----
## Bereinigen ----
bereinigen <- F

if (bereinigen) {
  n <- ncol(pp_origin)
  
  ### Bewerbungen bereinigen ----
  pp.ber <- 
    pp_origin %>%
    ### dispens Bewerbungen 
    mutate(across(c(q15_1, q15_2), ~ as.numeric(.x)),
           q15_1_kat = cut(q15_1 %>% as.numeric(), breaks = c(-1, 3, 5, 10, 20, 200), 
                           labels = c("bis 3 Bewerbungen", "4-5", "6-10", "11-20", "mehr als 20 Bewerbungen")), 
           q15_anteil_antworten = (q15_2 / q15_1) * 100) %>% 
    ### pp Bewerbungen
    mutate(
      across(matches("q18_|q31"), as.numeric),
      q18_anz_bew = if_else(q18_1 > 300, NA, q18_1),
      q18_anz_bew_kat = cut(q18_anz_bew, breaks = c(-1, 0, 1, 3, 5, 10, 15, 20, 30, 50, 100, max(q18_anz_bew, na.rm = T)), 
                            labels = c("Keine Bewerbung", "1 Bewerbung", "2-3 Bewerbungen", "4-5 Bewerbungen", "6-10 Bewerbungen", "11-15 Bewerbungen", "16-20 Bewerbungen", "21-30 Bewerbungen", "31-50 Bewerbungen", "51-100 Bewerbungen", "Mehr als 100 Bewerbungen")),
      q18_anz_bew_kat2 = cut(q18_anz_bew, breaks = c(-1, 1, 3, 5, 10, 20, max(q18_anz_bew, na.rm = T)), 
                             labels = c("bis 1 Bewerbung", "2-3 Bewerbungen", "4-5 Bewerbungen", "6-10 Bewerbungen", "11-20 Bewerbungen", "mehr als 20 Bewerbungen")),
      q18_anteil_antworten = (q18_2 / q18_1) * 100,
      # q14_corona_absagegrund = (q14a / q14_2) * 100, # gibts nicht mehr
      q31_ueberstunden = cut(q31, breaks = c(-1, 20, 35, 40, max(q31, na.rm = T)), 
                             labels = c("bis 20 Stunden", "21-35 Stunden", "36-40 Stunden", "Mehr als 40 Stunden")))
      # q58_notendurchschnitt = (as.numeric(q58_1) + as.numeric(q58_2) + as.numeric(q58_3))/3), # gibts nicht mehr 
      # q58_notendurchschnitt = cut(q58_notendurchschnitt, breaks = c(-1, 2, 3, 4, 5, 6.01), labels = c("Sehr gut", "Gut", "Befriedigend", "Genügend", "Nicht Genügend")), # gibts nicht mehr
      
  
  ## Einkommen bereinigen ----
  pp.ber %>% 
    select(q38_1, q38_2, q38_3) %>% 
    mutate_all(as.numeric) %>% 
    describe()
  
  outl1 <- 
    pp.ber %>% 
    select(q38_1, q38_2, q38_3) %>% 
    mutate_all(as.numeric) %>% 
    map(fnc.outliers)
  outl1$q38_2$Wertebereich
    
  pp.ber %>% 
    select(q38_1, q38_2, q38_3) %>% 
    mutate_all(as.numeric) %>% 
    pivot_longer(-0) %>% 
    ggplot(aes(x = value)) +
    geom_histogram() +
    facet_grid(~ name)
  
  pp.ber <-
  pp.ber %>%
    mutate(across(matches("q38|q37"), as.numeric)) %>% 
    mutate(q38_einkommen = if_else(q38_1 > 5000 | q38_1 < 0, NA_real_, q38_1),
           q38_einkommen = if_else(q37 == "Nein", 0, q38_einkommen),
           q38_taschengeld = if_else(q38_2 > 2000 | q38_2 < 0, NA_real_, q38_2),
           q38_taschengeld = if_else(q37 == "Nein", 0, q38_taschengeld),
           q38_trinkgeld = if_else(q38_3 > 2000 | q38_3 < 0, NA_real_, q38_3),
           q38_trinkgeld = if_else(q37 == "Nein", 0, q38_trinkgeld), 
           q38_einkommen = if_else(!is.na(q38_taschengeld) & is.na(q38_einkommen), 0, q38_einkommen),
           q38_taschengeld = if_else(!is.na(q38_einkommen) & is.na(q38_taschengeld), 0, q38_taschengeld), 
           q38_trinkgeld = if_else(is.na(q38_trinkgeld), 0, q38_trinkgeld), 
           q38_trinkgeld = if_else(is.na(q38_einkommen) & is.na(q38_taschengeld) & is.na(q38_3), NA_real_, q38_trinkgeld), 
           q38_eink_gesamt = q38_einkommen + q38_taschengeld, 
           q38_einkommen_kat = cut(q38_einkommen, breaks = c(-1, 0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 5001), labels = c("Kein Einkommen", "1-250 Euro","251-500 Euro", "501-750 Euro", "751-1000 Euro", "1001-1250 Euro", "1251-1500 Euro", "1501-1750 Euro", "1751-2000 Euro", "Mehr als 2000 Euro")),
           q38_taschengeld_kat = cut(q38_taschengeld, breaks = c(-1, 0, 250, 500, 750, 1000, 2001), 
                                     labels = c("Kein Taschengeld", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro", "Mehr als 1000 Euro")),
           q38_trinkgeld_kat = cut(q38_trinkgeld, breaks = c(-1, 0, 250, 500, 750, 1000, 2001), labels = c("Kein Trinkgeld", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro", "Mehr als 1000 Euro")),
           q38_einkommen_gesamt_kat = cut(q38_eink_gesamt, breaks = c(-1, 0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 5001), labels = c("Kein Einkommen", "1-250 Euro","251-500 Euro", "501-750 Euro", "751-1000 Euro", "1001-1250 Euro", "1251-1500 Euro", "1501-1750 Euro", "1751-2000 Euro", "Mehr als 2000 Euro")),
           q38_einkommen_gesamt_kat_rec = case_when(q38_einkommen_gesamt_kat %>% str_detect("1251|1501|1751|Mehr") ~ "Mehr als 1250 Euro",
                                                    .default = q38_einkommen_gesamt_kat) %>% 
             factor(levels = c("Kein Einkommen",
                               "1-250 Euro",
                               "251-500 Euro",
                               "501-750 Euro",
                               "751-1000 Euro",
                               "1001-1250 Euro",
                               "Mehr als 1250 Euro")),
           q38_einkommen_systematik = case_when(q38_einkommen > 0 & q38_taschengeld > 0 & q38_trinkgeld > 0 ~ "Einkommen + Taschengeld + Trinkgeld", 
                                                q38_einkommen > 0 & q38_taschengeld > 0 & q38_trinkgeld == 0 ~ "Einkommen + Taschengeld", 
                                                q38_einkommen > 0 & q38_taschengeld == 0 & q38_trinkgeld > 0 ~ "Einkommen + Trinkgeld", 
                                                q38_einkommen == 0 & q38_taschengeld > 0 & q38_trinkgeld > 0 ~ "Taschengeld + Trinkgeld", 
                                                q38_einkommen > 0 & q38_taschengeld == 0 & q38_trinkgeld == 0 ~ "Einkommen", 
                                                q38_einkommen == 0 & q38_taschengeld > 0 & q38_trinkgeld == 0 ~ "Taschengeld", 
                                                q38_einkommen == 0 & q38_taschengeld == 0 & q38_trinkgeld > 0 ~ "Trinkgeld", 
                                                q38_einkommen == 0 & q38_taschengeld == 0 & q38_trinkgeld == 0 ~ "Keines",
                                                .default = "Sonstiges"),
           q38_einkommen_systematik = factor(q38_einkommen_systematik, levels = c("Einkommen", "Einkommen + Trinkgeld", "Einkommen + Taschengeld", "Einkommen + Taschengeld + Trinkgeld", "Taschengeld", "Taschengeld + Trinkgeld", "Trinkgeld", "Keines", "Sonstiges")), 
           q38_einkommen_systematik2 = case_when(q38_einkommen == 0 & q38_taschengeld == 0 ~ "Kein Einkommen", 
                                                 q38_einkommen > 0 & q38_taschengeld == 0 ~ "Einkommen", 
                                                 q38_einkommen == 0 & q38_taschengeld > 1 ~ "Taschengeld", 
                                                 q38_einkommen > 0 & q38_taschengeld > 0 ~ "Einkommen + Taschengeld", 
                                                 .default = "Sonstiges"),
           q38_einkommen_systematik2 = factor(q38_einkommen_systematik2, levels = c("Einkommen", "Taschengeld", "Einkommen + Taschengeld", "Kein Einkommen", "Sonstiges")),
    ) 
  ### Letzter Stand: q38_einkommen_systematik2 scheint die NAs nicht korrekt zu zeigen
  
  
  ## Kosten berechnen ----
  pp.ber <- 
    pp.ber %>% 
    mutate(across(c(q42, matches("q40_")), ~ .x %>% as.numeric()),
           q40_unterkunftskosten = if_else(is.na(q40_1), 0, q40_1),
           q40_unterkunftskosten = if_else(q40_unterkunftskosten >= 1000, NA_real_, q40_unterkunftskosten),
           q40_fahrtkosten = if_else(q40_2 >= 500, NA_real_, q40_2), 
           q40_essenskosten = if_else(q40_3 >= 500, NA_real_, q40_3),
           q40_unterkunftskosten = if_else(is.na(q38_einkommen) & is.na(q38_taschengeld) & is.na(q38_trinkgeld), NA_real_, q40_unterkunftskosten),
           q40_fahrtkosten = if_else(is.na(q38_einkommen) & is.na(q38_taschengeld) & is.na(q38_trinkgeld), NA_real_, q40_fahrtkosten),
           q40_essenskosten = if_else(is.na(q38_einkommen) & is.na(q38_taschengeld) & is.na(q38_trinkgeld), NA_real_, q40_essenskosten),
           across(c(q40_unterkunftskosten, q40_fahrtkosten, q40_essenskosten), ~ if_else(.x < 0, NA_real_, .x)), 
           q40_arbeitskosten = if_else(q41 == "Nein", 0, q42),
           q40_kosten_gesamt = q40_unterkunftskosten + q40_fahrtkosten + q40_essenskosten,
           q40_essenskosten_kat = cut(q40_essenskosten, breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
           q40_fahrtkosten_kat = cut(q40_fahrtkosten,  breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
           q40_arbeitskosten_kat = cut(q40_arbeitskosten, breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
           q40_kosten_gesamt_kat = cut(q40_kosten_gesamt, breaks = c(-1, 0, 250, 500, 750, 1001), labels = c("Keine Kosten", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro")),
           einkommen_kosten = q38_eink_gesamt - q40_kosten_gesamt)
  
  
  # saveRDS(pp.ber,"pp.ber.RData")
  
  
  ## Multiple Choice Items rekodieren  ----
  ## Hiermit hole ich mir die variablennamen all jener Fragen, die Multiple Choice sind
  limsurvey <- read.xlsx("../frabo/limesurvey_survey_994263.xlsx")
  
  multipl <- limsurvey %>% 
    filter(`type/scale` == "M") %>% 
    select(`type/scale`, name, text) %>% 
    as_tibble
  
  ### Die Variablengruppen werden mittels multipl$name zusammengefasst und als eigener DF behandelt, was die grudfenspezifische Berechnung der Totals erleichtert
  ### einzelne MC-Gruppen in Listenelementen zusammenfassen
  mult.df.1 <-
    map(multipl$name,
        ~ pp.ber %>%
          mutate(
            across(matches(paste0(multipl$name, collapse = "|")), ~ fct_drop(.x, only = "other")) # ist wahrscheinlich eine alte Codierung, als Limesurvey in der Untervariable vermerkte, dass ein "other" angekreuzt wurde. Kann drinbleiben.
          ) %>%
          select(id, matches(.x))
    )
  ### total und _other2 berechnen
  mult.df.2 <-
    map2(mult.df.1, multipl, ~
           .x %>%
           mutate(
             ### zunächst alle NAs in _other auf "Nicht Gewählt"
             across(matches("_other"), ~ case_when(.x == "Nicht gewählt" ~ "Nicht gewählt",
                                                   .x != "" ~ "Ja",
                                                   .default = "Nicht gewählt"),
                    .names = "{.col}2"),
             across(matches("_other"), ~ factor(.x, levels = c("Nicht gewählt", "Ja"))),
             ### Total berechnen, um im Nachhinein NA zu vergeben
             total = rowSums(across(-c(id, matches("_other$"))) == "Ja", na.rm = T),
             across(matches("other2"), ~ if_else(total == 0, NA_character_, as.character(.x))), # wenn total == 0, dann alle _other2 == NA
           ) %>%
           select(id, matches("total|other2")) %>%
           rename(!!paste0(.y, "_", "total") := "total") %>%
           copy_labels(lm_original)
    ) %>%
    reduce(left_join, by = "id")
  
  # Neue Variablen an df anfügen
  pp.ber <- pp.ber %>%
    left_join(mult.df.2, by = "id")
  
  
  
  ## Other gesammelt recodieren
  # pp.ber <- pp.ber %>% mutate(across(.cols = c(q20), .fns = ~fct_recode(.x, "Anderes" = "other")))
  
  
  #### Sonderbehandlung übernommen von Martin ----
  sonder <- "q17" # kann sich ändern
  sonder.1 <- paste0("q",
                     (sonder %>% str_remove("\\w") %>% as.numeric())-1) # die entsprechende Variable sollte sich eine Position davor befindet
  
  lbs <- pp.ber %>% # labels kopieren, um sie später wieder anzufügen
    select(matches(paste0(sonder,"_\\d+$"))) %>%
    get_label
  
  pp.ber <-
  pp.ber %>%
    mutate(!!sym(paste0(sonder,"_other","2")) := if_else(!!sym(paste0(sonder,"_other")) != "", "Ja", NA_character_),
           !!sym(paste0(sonder,"_other","2")) := factor(!!sym(paste0(sonder,"_other","2")), levels = c("Nicht Gewählt", "Ja")),
           across(c(matches(paste0(sonder,"_\\d+$")),
                    paste0(sonder,"_other","2")), ~if_else(!!sym(paste0(sonder,"_total")) == 0 | (!!sym(sonder.1) == "Sehr leicht" | !!sym(sonder.1) == "Leicht"),
                                                           NA_character_ , .x)),
           across(c(paste0(sonder,"_other","2")), ~if_else(!!sym(paste0(sonder,"_total")) != 0 & !!sym(paste0(sonder,"_other","2")) %>% is.na,
                                                           "Nicht Gewählt" , .x)))
  
  
  #### Variablenlabels von _other auf _other2 kopieren ----
  x <- data.frame(name = pp.ber %>%
                    select(matches("_other2$")) %>% 
                    names) %>% 
    mutate(strng = name %>% str_extract(".+(?=\\_.+)")) %>% 
    as_tibble
  
  y <- pp.ber %>% 
    select(matches("_other$")) %>% 
    get_label %>% 
    as.list %>% 
    as.data.frame %>% 
    t %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    as_tibble %>% 
    rename(name = rowname,
           label = V1) %>% 
    mutate(strng = name %>% str_extract(".+(?=\\_.+)"))
  
  z <- x %>% 
    left_join(y, by="strng")
  
  pp.ber[z$name.x] <- set_label(pp.ber[z$name.x], z$label)
  
  
  ## schultyp_rec mit Sozialberufen ----
  pp.ber <- pp.ber %>%
    mutate(schultyp_rec = case_when(
      q02bhs %>% str_detect("Elementarpädagogik") ~ paste0("Höhere ",q02bhs),
      q02bhs %>% str_detect("Kaufmän") ~ "Handelsakademie (HAK)",
      q02bhs %>% str_detect("HTL|Mode") ~ "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
      q02bhs %>% str_detect("HLW") ~ "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
      q02bhs %>% str_detect("Tourism") ~ "Höhere Lehranstalt für Tourismus (HLT)",
      q02bms %>% str_detect("Handelsschule") ~ "Handelsschule (HAS)",
      q02bms %>% str_detect("Fachschule für wirtschaftliche Berufe") ~ "Fachschule für wirtschaftliche Berufe",
      q02bms %>% str_detect("Hotel|Tourism") ~ "Hotelfachschule, Tourismusfachschule",
      q02bms %>% str_detect("Sozial") ~ "Dreijährige Fachschule für Sozialberufe",
      q02bms %>% str_detect("Pädag") ~ "Dreijährige Fachschule für Pädagogische Assistenz", # wurden herausgefiltert
      is.na(q02bms) & is.na(q02bhs) ~ NA,
      q02bms %>% str_detect(paste0(c("Handelsschule", # das, was nach den vorherigen BMS übrigbleibt
                                     "wirtschaftliche Berufe",
                                     "Hotel",
                                     "Tourism",
                                     "Sozial",
                                     "[P-p]ädag"), collapse = "|"), negate = T) ~ "Gewerbliche, technische und kunstgew. Fachschulen"),
      # q02bms %>% str_detect("Bautechnik|Bildhauerei|Büchsenmacher|Chemie|Chemische Technologie|Drechsler|Elektronik und Technische Informatik|Elektrotechnik|Flugtechnik|Gebäudetechnik|Glastechnik und Gestaltung|Holzwirtschaft|Informationstechnik|Informationstechnik|Keramik und Ofenbau|Korb- und Möbelflechterei|Lederdesign|Malerei und Gestaltung|Maschinenbau|Mechatronik|Mediengestaltung und digitale Druckproduktion|Präzisions- und Uhrentechnik|Steintechnik und Steingestaltung|Streich- und Saiteninstrumentenerzeugung|Tischlerei|Vergolden und Schriftdesign|Weberei") ~ "Gewerbliche, technische und kunstgew. Fachschulen"),
      
      ausbildungsfeld = case_when(q02bhs == "Bildungsanstalt für Elementarpädagogik oder Sozialpädagogik" | q02bms %>% str_detect("ädagog") ~ 'Bildungsanstalt pädagogisch',
                                  q02bhs %>% str_detect("wirtschaft") | q02bms %>% str_detect("wirtschaft") ~ 'Wirtschaftliche Schule',
                                  q02bhs %>% str_detect("Hotel|Tourismus
                                Hotel|Tourismus") | q02bms %>% str_detect("Hotel|Tourismus
                                                                          Hotel|Tourismus") ~ "Tourismus",
                                  ### Sollte eine Eingliederung von FS für Päd. in Päd gewünscht sein:
                                  # q02bhs == "Bildungsanstalt für Elementarpädagogik oder Sozialpädagogik" | q02bms %>% str_detect("[P-p]ädag") ~ 'Bildungsanstalt "pädagogisch"',
                                  # q02bhs %>% str_detect("Hotel|Mode|Tourismus|wirtschaft") | q02bms %>% str_detect("Hotel|Mode|Tourismus|wirtschaft|Sozial") ~ 'Humanberufliche Schule',
                                  q02bhs == "Kaufmännische Schule (HAK)" | q02bms %>% str_detect("Handelsschule") ~ 'Kaufmännische Schule',
                                  q02bhs %>% str_detect("[S-s]ozial") | q02bms %>% str_detect("[S-s]ozial") ~ 'Soziales',
                                  # q02bhs %>% str_detect("fors") ~ 'Land- und forstwirtschaftliche Schule',
                                  q02bhs %>% str_detect("gewerbl|Mode") | 
                                    q02bms %>% str_detect(paste0(c("Handelsschule", # das, was nach den vorherigen BMS übrigbleibt
                                                                   "wirtschaftliche Berufe",
                                                                   "Hotel",
                                                                   "Tourism",
                                                                   "Sozial",
                                                                   "[P-p]ädag"), collapse = "|"), negate = T) ~ 'Technisch gewerbliche Schule'))
  
  
  ### Levels verändern
  pp.ber <- pp.ber %>% 
    # select(schultyp_rec) %>% unique
    mutate(schultyp_rec = factor(schultyp_rec, levels = c("Handelsakademie (HAK)",
                                                          "Handelsschule (HAS)",
                                                          "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
                                                          "Gewerbliche, technische und kunstgew. Fachschulen",
                                                          "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
                                                          "Fachschule für wirtschaftliche Berufe",
                                                          "Höhere Lehranstalt für Tourismus (HLT)",
                                                          "Hotelfachschule, Tourismusfachschule",
                                                          "Höhere Bildungsanstalt für Elementarpädagogik oder Sozialpädagogik",
                                                          # "Dreijährige Fachschule für Pädagogische Assistenz",
                                                          "Dreijährige Fachschule für Sozialberufe"))) 
  
  
  pp.ber$schultyp_rec <- pp.ber$schultyp_rec %>% droplevels
  
  ### q23_ haben so viele Abfragen (n=13), dass ein Index spannend wäre: Wie viel Unterstützung erhält man?
  pp.ber <- pp.ber %>% 
    mutate(across(matches("q23"), ~ if_else(.x == "Ja", 1, 0), .names = "{.col}_n"),
           q23_unterstuetzung = rowSums(across(matches("q23.+_n")), na.rm=T))
  
  
  ## Indexvariablen ----
  # index.var <- var.finder.label.fnc(pp,"Wie beurteilen Sie rückblickend das Pflichtp", "pp3")$variable # %>%  str_c("_n")
  
  # pp.ber <- pp.ber %>% 
  #   mutate(across(all_of(index.var), ~ .x %>% 
  #                   str_extract("^\\d") %>% 
  #                   as.numeric, .names = "{.col}_n"))
  
  ### Speichern
  # saveRDS(pp.ber, "pp.ber.RData")
  
  
  ## Rating Variables with endpoint labels to numeric ----
  rating.varnames <- map(pp.ber, ~ .x %>%
                           get_labels %>% 
                           na.omit %>% 
                           str_detect("(?!.*jährig)^\\d - ")) %>% # "- jährig" kommt in schultypen vor 
    keep(any) %>% 
    names %>% 
    str_remove("(?=\\_).*") %>%
    unique
  
  pp.ber <- pp.ber %>% 
    mutate(across(matches(rating.varnames), ~ as.numeric(.x), 
                  .names = "{.col}_n"))
  
  # saveRDS(pp.ber, "pp.ber.RData")
  

  ## Rekodieren ----
  # pp.ber <- 
  pp.ber %>%
    # frq(schultyp_rec)
    # mutate(!!sym(variables[10,1] %>% unlist) := fct_recode(!!sym(variables[10,1] %>% unlist),
  ### variables.df ist eine veraltete Library. 
    mutate(schultyp_rec = fct_recode(schultyp_rec,
                                     "BHS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAK"),
                                     "BMS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAS"),
                                     "BHS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("HTL"),
                                     "BMS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("techn.+Fachschule"),
                                     "BHS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("HLW"),
                                     "BMS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("Fachschule.+wirtschaft"),
                                     "BHS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("HLT"),
                                     "BMS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("Hotelfachschule"),
                                     "BHS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("Höhere.+ädag"),
                                     # "BMS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("BMS.+ädag"),
                                     # "BHS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("BHS.+Sozial"),
                                     "BMS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("Sozialberuf"),
                                     NULL = "Dreijährige Fachschule für Pädagogische Assistenz") %>% 
             fct_relevel("BHS (kaufm.)",
                         "BMS (kaufm.)",
                         "BHS (techn.)",
                         "BMS (techn.)",
                         "BHS (wirtschaft.)",
                         "BMS (wirtschaft.)",
                         "BHS (tourist.)",
                         "BMS (tourist.)",
                         "BHS (pädag.)",
                         # "BMS (pädag.)",
                         # "BHS (sozial)",
                         "BMS (sozial)"),
           mutate(q56_alt = q56,
                    q56 = if_else(q55 == "Ja" & q57 == "Ich werde mir einen Arbeitsplatz suchen.", q56, NA)), 
           q24 = factor(q24, levels = c("Regional", "Überregional", "Im Ausland")),
           q28 = factor(q28, levels = c("1-4 MA",
                                        "5-9 MA",
                                        "10-49 MA",
                                        "50-99 MA",
                                        "100-250 MA",
                                        "Über 250 MA")),
           q57 = factor(q57, c("Suche nach Arbeitsplatz", 
                               "Weiterführende Ausbildung", 
                               "Weiß noch nicht genau")),
           q58 = factor(q58, c("Ja",  
                               "Mit Ausbildungsrichtung verwandt", 
                               "Nein",
                               NULL
                               )),
           q58 = factor(q58, c("Ja",  
                               "Mit Ausbildungsrichtung verwandt", 
                               "Nein",
                               NULL
           )),
           richtung_geschlecht = fct_cross(q59,
                                           ausbildungsfeld, sep = " - "),
           ### Level von Geschlecht-Richtung
           richtung_geschlecht = fct_recode(richtung_geschlecht,
                                            "Männlich - Pädagogisch"    = 'Männlich - Bildungsanstalt pädagogisch',
                                            "Weiblich - Pädagogisch"    = 'Weiblich - Bildungsanstalt pädagogisch',
                                            "Männlich - Kaufmännisch"    = "Männlich - Kaufmännische Schule",
                                            "Weiblich - Kaufmännisch"    = "Weiblich - Kaufmännische Schule",
                                            "Männlich - Wirtschaftlich"    = "Männlich - Wirtschaftliche Schule",
                                            "Weiblich - Wirtschaftlich"    = "Weiblich - Wirtschaftliche Schule",
                                            "Männlich - Techn.-gewerbl." = "Männlich - Technisch gewerbliche Schule",
                                            "Weiblich - Techn.-gewerbl." = "Weiblich - Technisch gewerbliche Schule") %>% 
             fct_relevel(richtung_geschlecht, c("Männlich - Kaufmännisch",
                                                "Weiblich - Kaufmännisch",
                                                "Männlich - Techn.-gewerbl.",
                                                "Weiblich - Techn.-gewerbl.",
                                                "Männlich - Wirtschaftlich",
                                                "Weiblich - Wirtschaftlich",
                                                "Männlich - Tourismus",
                                                "Weiblich - Tourismus",
                                                "Männlich - Pädagogisch",
                                                "Weiblich - Pädagogisch",
                                                "Männlich - Soziales",
                                                "Weiblich - Soziales")),
           q61 = factor(q61, levels = c(
             "Nur Deutsch", 
             "Deutsch und andere Sprache(n)",
             "Andere Sprache(n) und Deutsch",
             "Nur andere Sprache(n) als Deutsch")),
           q64_sozialer_hintergrund = fct_collapse(q64, 
                                                   "Niedrig" = "Pflichtschule (Hauptschule, Neue Mittelschule, Polytechnische Schule etc.)", 
                                                   "Mittel" = c("Lehre, Berufsschule", "Berufsbildende mittlere Schule (Fachschule, Handelsschule, Schule für Sozialbetreuungsberufe etc.)"),
                                                   "Gehoben" = c("Matura (Gymnasium, HTL, HAK, HBLA, BAfEP/BASOP), Berufsreifeprüfung", "Meisterprüfung, Werkmeister"),
                                                   "Hoch" = "Universität, Fachhochschule, Pädagogische Hochschule, Akademie"),
           total = "-")
  
  ## Branchen als Break inkludieren: levels mit n<35 löschen --> Person erhält "Sonstige"
  # pp <- pp %>%
  #   mutate(q27_alt = q27) 
  ex.branche <- pp.ber %>%
    count(q27, wt=gewicht) %>% 
    filter(n < 35) %>% 
    select(q27) %>% 
    unlist() %>% 
    as.vector()
  
  pp.ber <-
    pp.ber %>%
    mutate(q27_rec = if_else(q27 %in% ex.branche, "Sonstige", q27) %>% 
             fct_relevel("Sonstige", after = Inf)) %>% 
    # count(q27_rec, q27)
    var_labels(q27_rec = "In welchem Berufsbereich haben Sie Ihr Praktikum absolviert?")
  

  ## Gültige Fälle ----
  ### Dispens und PP ----
  ### Teile Datensatz in jene, die Praktikum absolviert haben und jene mit Dispens
  pp_dispens <- 
    pp.ber %>% 
    filter(q08 == "wurde mir komplett erlassen (Dispens).")
  saveRDS(pp_dispens, "pp_dispens.RData")
  
  # ohne dispens
  pp.odp <-
    pp.ber %>%
    mutate(dispens = case_when(q08 == ("wurde mir komplett erlassen (Dispens).") ~ T, 
                               q08 %>% is.na ~ F,
                               .default = F))  %>% 
    filter(dispens == F)
  
  
  ### ungewoehnliche Werte ----
  
  # pp.odp <- readRDS("pp.odp.RData")
  
  ### Erwachsene in Ausbildung
  # pp.odp %>% 
  #   filter(q60 > 21) %>% 
  #   # select(matches("q05")) %>% 
  #   group_by(across(matches("q05"))) %>% 
  #   reframe(q60,
  #           q02bms,
  #           q02bhs,
  #           n=n()) %>% 
  #   filter(n > 1) %>% 
  #   reframe(sum = sum(n))
  #   write.csv("temp.csv", row.names = F)
  # system("open temp.csv")
  
  
  # ### Alter bereinigen
  # pp.odp <- pp.odp %>%
  #   mutate(q60 = if_else(q60 >= 2000, 2023-q60, q60),
  #          q60_rec = case_when(q60 < 15 ~ NA,
  #                              q60 > 22 ~ NA, 
  #                              .default = q60) %>% 
  #            as.integer,
  #          q60_filter = case_when(q60 < 15 ~ F,
  #                                 q60 > 22 ~ F, 
  #                                 .default = T))# %>% 
  # frq(q60_rec, q60_filter) #%>%
  # View
  # frq(q60_rec)
  
  # ### missing-muster
  # misscumsum <- pp.odp %>%
  #   # frq(itemnonr)
  #   filter(itemnonr >= 100) %>% 
  #   mutate(across(everything(), ~ as.character(.x))) %>% 
  #   pivot_longer(-id, names_to = "variable") %>% 
  #   filter(variable %in% names(pp.odp_origin)) %>% 
  #   group_by(id) %>% 
  #   mutate(miss = if_else(value %>% is.na, 1, 0),
  #          misscum = cumsum(miss)) %>% 
  #   ungroup 
  
  # misscumsum %>% 
  #   # filter(misscum > 200) %>% 
  #   # View
  #   mutate(id1 = id %>% as.character) %>% 
  #   ggplot(aes(x = variable, y = misscum, group = id)) +
  #   geom_line() ## --> macht seltsame linien
  
  # saveRDS(pp.odp, "pp.odp_bereinigt.RData")
  
  ### Filtern ----
  
  # pp.odp <- readRDS("pp.odp.RData")
  
  
  ### "Dreijährige Fachschule für Pädagogische Assistent/inn/en" wegen geringer Fallzahl entfernen.
  pp.odp <- pp.odp %>%
    mutate(filter.paed.ass = case_when(q02bms %>% str_detect("Dreijährige Fachschule für Pädagogische Assistent/inn/en") ~ F, 
                                       q02bms %>% is.na ~ T,
                                       .default = T)) 
  aus1 <- pp.odp %>% 
    filter(filter.paed.ass == F) %>% 
    nrow
  ### Mindestens vorletzte Seite erreichen
  aus2 <- pp.odp %>%
    filter(lastpage < 10) %>% 
    nrow
  ### Personen ohne Angabe von Schuladresse (und daher auch auch ohne Bundesland) ausschließen
  aus3 <- pp.odp %>%
    filter(Bundesland %>% is.na) %>% 
    nrow
  ### Land- und Forstwirtschaftliche Schulen
  aus4 <- pp.odp %>%
    filter(q02bhs == "Land- und forstwirtschaftliche Schule") %>% 
    nrow
  ### Dispens
  aus5 <- pp_dispens %>% 
    nrow
  ### Alter 15 - 22
  aus6 <- pp.odp %>% 
    filter(q60_filter == F) %>% 
    nrow
  
  pp <- pp.odp %>%
    # "Dreijährige Fachschule für Pädagogische Assistent/inn/en" wegen geringer Fallzahl entfernen.
    filter(filter.paed.ass) %>% 
    # Mindestens vorletzte Seite erreichen
    filter(lastpage >=10) %>% 
    # Personen ohne Angabe von Schuladresse (und daher auch auch ohne Bundesland) ausschließen
    filter(Bundesland %>% is.na == F) %>% 
    # Land- und Forstwirtschaftliche Schulen
    mutate(filter.forst = case_when(q02bhs == "Land- und forstwirtschaftliche Schule" ~ F, 
                                    q02bhs %>% is.na ~ T,
                                    .default = T)) %>% 
    filter(filter.forst) %>% 
    # Alter 15 - 22
    filter(q60_filter == T) %>% 
    select(-c(filter.paed.ass, filter.forst, q60_filter))
  
  ### Ausschluss ----
  ausschluss <- tibble(`Ausschluss (Reihenfolge)` = c("Personen mit Ausbildung: Dreijährige Fachschule für Pädagogische Assistent/inn/en",
                                                      "Personen, die nicht auf die nicht auf die letzte Seite gekommen sind",
                                                      "Personen ohne Angaben von Schuladresse und damit Bundesland",
                                                      "Personen in BHS Land- und Forstwirtschaft --> tw. anderes Ministerium",
                                                      "Dispens",
                                                      "Alter unter 15 und über 22 Jahre",
                                                      "Gesamt"),
                       Anzahl = c(    aus1, aus2, aus3, aus4, aus5, aus6,
                                      sum(aus1, aus2, aus3, aus4, aus5, aus6)),
                       Brutto = c(rep("",6), pp.df_origin %>% nrow),
                       Netto = c(rep("",6), pp.df %>% nrow))
  
  saveRDS(ausschluss, "ausschluss.RData")
  write.csv(ausschluss, row.names = F, "ausschluss.csv")
  saveRDS(pp, "pp_bereinigt.RData")
  
  
  ## Gewichtung ----
  # source("gewichtung.R", encoding="utf-8")
  gewicht <- readRDS("gewicht.RData")
  ### gewicht (aus gewichtung.R) wird mittels left_join an pp.odp angebunden und somit werden die Fälle entsprechend gewichtet 
  pp <- pp %>%
    select(-gewicht) %>%
    left_join(gewicht, by = c("schultyp_rec","Bundesland"))
  
  ### Levels verändern, weil Gewichts-schultyp_rec nicht Faktor ist
  pp <- pp %>% 
    # select(schultyp_rec) %>% unique
    mutate(schultyp_rec = factor(schultyp_rec, levels = c("Handelsakademie (HAK)",
                                                          "Handelsschule (HAS)",
                                                          "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
                                                          "Gewerbliche, technische und kunstgew. Fachschulen",
                                                          "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
                                                          "Fachschule für wirtschaftliche Berufe",
                                                          "Höhere Lehranstalt für Tourismus (HLT)",
                                                          "Hotelfachschule, Tourismusfachschule",
                                                          "Höhere Bildungsanstalt für Elementarpädagogik oder Sozialpädagogik",
                                                          # "Dreijährige Fachschule für Pädagogische Assistenz",
                                                          "Dreijährige Fachschule für Sozialberufe"))) 
  
  pp$schultyp_rec <- pp$schultyp_rec %>% droplevels
  
  
  ### Kontrolle: diese Tabelle sollte mit Gewichtungstabelle.xlsx aus gewichtung.R übereinstimmen
  saveRDS(pp,"pp.RData")
  
  
  ## Dimensionsreduktion ----
  ### Factorenanalyse ----
  
  # index.var <- var.finder.label.fnc(pp,"Wie beurteilen Sie rückblickend das Pflichtp", "pp3")$variable %>% 
  #   str_c("_n")
  # 
  # fact_data <- pp %>% select(all_of(index.var))
  # 
  # KMO(r=cor(na.omit(fact_data)))
  # cortest.bartlett(fact_data)
  # 
  # 
  # fafitfree <- fa(fact_data, nfactors = ncol(fact_data), rotate = "none")
  # n_factors <- length(fafitfree$e.values)
  # scree     <- data.frame(
  #   Factor_n =  as.factor(1:n_factors), 
  #   Eigenvalue = fafitfree$e.values)
  # ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  #   geom_point() + geom_line() +
  #   xlab("Number of factors") +
  #   ylab("Initial eigenvalue") +
  #   labs( title = "Scree Plot", 
  #         subtitle = "(Based on the unreduced correlation matrix)")
  
  # Factorenanalyse durchführen
  # fa_res <- fa(fact_data, nfactors = 2, rotate = "oblimin")
  # fa.parallel(fact_data)
  
  ## Indikatoren erstellen
  # pp <- pp %>% mutate(ind_qual_arbeitsklima = rowMeans(across(c(q42_1_n, q42_2_n))), 
  #                       ind_qual_rahmenbed = rowMeans(across(c(q42_3_n, q42_4_n, q42_5_n))),
  #                       ind_qual_aufgaben = rowMeans(across(c(q42_7_n, q42_8_n, q42_9_n, q42_10_n))),
  #                       ind_qual_bezahlung = q42_6_n)
  
  
  ### Clusteranalyse ----
  ### Todo
  ## Syntax für K-Means Clusteranalyse: Herausforderungen: Missing Values, das ist der mögliche Weg dazu, über Reihennummer.
  index.var <- var.finder.label.fnc(pp,"Wie beurteilen Sie rückblickend das Pflichtp")$variable %>% 
    str_c("_n")
  
  ### Cluster Lösung
  clust_data <- pp %>% select(id, all_of(index.var)) %>% as.data.frame()
  # fviz_nbclust(na.omit(clust_data %>% select(-id)), kmeans, method = "silhouette")
  # fviz_nbclust(na.omit(clust_data %>% select(-id)), kmeans, method = "wss")
  
  set.seed(1234)
  # um Fälle mit missing herauszufilter
  nonmis <- clust_data %>%
    filter(if_all(everything(),~ !is.na(.x))) %>%
    select(id) %>%
    mutate(cluster_qualitaet_4 = NA,
           cluster_qualitaet_3 = NA,
           cluster_qualitaet_2 = NA)
  
  kc.4 <- kmeans(x = na.omit(clust_data %>% select(-id)), 4)$cluster
  # kc.4 %>% str
  kc.3 <- kmeans(x = na.omit(clust_data %>% select(-id)), 3)$cluster
  # kc.3 %>% str
  kc.2 <- kmeans(x = na.omit(clust_data %>% select(-id)), 2)$cluster
  # kc.2 %>% str
  
  y <- nonmis %>% 
    mutate(cluster_qualitaet_4 = kc.4 %>% as.factor,
           cluster_qualitaet_3 = kc.3 %>% as.factor,
           cluster_qualitaet_2 = kc.2 %>% as.factor) %>% 
    # full_join(mis) %>% 
    arrange(id)
  
  pp <- pp %>% 
    select(-matches("Cluster"))
  
  pp <- pp %>% 
    full_join(y, by = "id")
  
  
  ## Zuordnung Klassen zu Ordinalniveau
  pp %>% 
    select(cluster_qualitaet_4,matches("q45.+_n")) %>% 
    pivot_longer(-cluster_qualitaet_4) %>%
    group_by(cluster_qualitaet_4,name) %>% 
    reframe(mean = mean(value,na.rm = T)) %>% 
    na.omit %>% 
    ggplot(aes(x=name, y=mean, group = cluster_qualitaet_4)) +
    geom_line(aes(color = cluster_qualitaet_4)) +
    # theme_bw() +
    theme(text = element_text(size = 14),
          legend.position="bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line = element_line(size = .3),
          panel.background = element_blank())
  
  pp %>% 
    select(cluster_qualitaet_3,matches("q45.+_n")) %>% 
    pivot_longer(-cluster_qualitaet_3) %>%
    group_by(cluster_qualitaet_3,name) %>% 
    reframe(mean = mean(value,na.rm = T)) %>% 
    na.omit %>% 
    ggplot(aes(x=name, y=mean, group = cluster_qualitaet_3)) +
    geom_line(aes(color = cluster_qualitaet_3)) +
    # theme_bw() +
    theme(text = element_text(size = 14),
          legend.position="bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line = element_line(size = .3),
          panel.background = element_blank())
  
  pp %>% 
    select(cluster_qualitaet_2,matches("q45.+_n")) %>% 
    pivot_longer(-cluster_qualitaet_2) %>%
    group_by(cluster_qualitaet_2,name) %>% 
    reframe(mean = mean(value,na.rm = T)) %>% 
    na.omit %>% 
    ggplot(aes(x=name, y=mean, group = cluster_qualitaet_2)) +
    geom_line(aes(color = cluster_qualitaet_2)) +
    # theme_bw() +
    theme(text = element_text(size = 14),
          legend.position="bottom",
          axis.title.x = element_blank(),
          axis.title.y = element_blank(),
          axis.line = element_line(size = .3),
          panel.background = element_blank())
  
  pp <- pp %>%
    mutate(
      cluster_qualitaet_2 = case_when(
        cluster_qualitaet_2 == 2 ~ "Gruppe A (gut)",
        cluster_qualitaet_2 == 1 ~ "Gruppe B (schlecht)"
      ) %>% factor,
      cluster_qualitaet_3 = case_when(
        cluster_qualitaet_3 == 3 ~ "Gruppe A (gut)",
        cluster_qualitaet_3 == 1 ~ "Gruppe B (mittel)",
        cluster_qualitaet_3 == 2 ~ "Gruppe C (schlecht)"
      ) %>% factor,
      cluster_qualitaet_4 = case_when(
        cluster_qualitaet_4 == 2 ~ "Gruppe A (sehr gut)",
        cluster_qualitaet_4 == 1 ~ "Gruppe B (gut)",
        cluster_qualitaet_4 == 3 ~ "Gruppe C (schlecht)",
        cluster_qualitaet_4 == 4 ~ "Gruppe D (sehr schlecht)"
      ) %>% factor
    )
  
  # ### Umpolen
  # pp <- pp %>%
  #   mutate(
  #     cluster_qualitaet_2 = case_when(
  #       cluster_qualitaet_2 == "Gruppe A (gut)" ~ "Gruppe B (schlecht)",
  #       cluster_qualitaet_2 == "Gruppe B (schlecht)" ~ "Gruppe A (gut)"
  #     ) %>% factor,
  #     cluster_qualitaet_3 = case_when(
  #       cluster_qualitaet_3 == "Gruppe A (gut)" ~ "Gruppe C (schlecht)",
  #       cluster_qualitaet_3 == "Gruppe B (mittel)" ~ "Gruppe B (mittel)",
  #       cluster_qualitaet_3 == "Gruppe C (schlecht)" ~ "Gruppe A (gut)",
  #     ) %>% factor,
  #     cluster_qualitaet_4 = case_when(
  #       cluster_qualitaet_4 == "Gruppe A (sehr gut)" ~ "Gruppe D (sehr schlecht)",
  #       cluster_qualitaet_4 == "Gruppe B (gut)" ~ "Gruppe C (schlecht)",
  #       cluster_qualitaet_4 == "Gruppe C (schlecht)" ~ "Gruppe B (gut)",
  #       cluster_qualitaet_4 == "Gruppe D (sehr schlecht)" ~ "Gruppe A (sehr gut)"
  #     ) %>% factor
  #   ) 
  
  
  ## Setting labels
  pp[c("cluster_qualitaet_2","cluster_qualitaet_3","cluster_qualitaet_4")] <- set_label(pp[c("cluster_qualitaet_2","cluster_qualitaet_3","cluster_qualitaet_4")], 
                                                                                        c(paste0("Qualität der Rahmenbedingungen",
                                                                                                 # "(",index.var %>% str_extract("^[a-z]\\d+") %>% unique, "_)",
                                                                                                 " mit 2 Clustern"),
                                                                                          paste0("Qualität der Rahmenbedingungen",
                                                                                                 # "(",index.var %>% str_extract("^[a-z]\\d+") %>% unique, "_)",
                                                                                                 " mit 3 Clustern"),
                                                                                          paste0("Qualität der Rahmenbedingungen",
                                                                                                 # "(",index.var %>% str_extract("^[a-z]\\d+") %>% unique, "_)",
                                                                                                 " mit 4 Clustern")))
  
  ### Save
  saveRDS(pp,"pp.RData")
  
  
  ## Attribute und Labels ----
  ### prüfen, welche Variablen keine Beschriftung haben
  pp %>%
    map(~ get_label(.x)) %>%
    keep(~ length(.x) == 0) %>%
    names()
  
  # labs <- tibble(string = attr.var,
  #                labl = attr.label)
  # 
  # ### extracting new colnames: von den neu hinzugefügten Variablen gibt es welche, die beschriftet werden sollten
  # nam <- pp[(n+1):ncol(pp)] %>% 
  #   names
  # 
  # nam.list <- map(c("^(?!.*_n|.*_total|.*_other)",# das sind einkommens- und kostenvariablen
  #                   "_n$","_total$","_other2"), ~ nam %>% 
  #                   str_subset(.x))
  # 
  # ### Abgleich neuer Variablen, die bereits ein Label besitzen
  # strng <- attr.var[attr.var %in% str_extract(nam.list[[1]],"(?<=q.{0,2})_.+")]
  # 
  # labs <- labs %>%
  #   filter(string %in% strng) %>% 
  #   mutate(var = pp %>% select(matches(string)) %>% names)
  # 
  # x <- pp %>% 
  #   select(matches("q\\d+_other$")) %>% 
  #   names %>% 
  #   str_replace("other","1$") %>% 
  #   paste0(collapse = "|")
  # 
  # 
  # ### Labels vergeben
  # pp[labs$var] <- set_label(pp[labs$var],
  #                           labs$labl)
  
  pp <-
    pp %>% 
    var_labels(
      "_anz_bew_kat" = "Anzahl Bewerbungen",
      "_anz_bew_kat2" = "Anzahl Bewerbungen",
      "_anteil_antworten" = "[Anteil der Rückmeldungen] Bewerbungen",
      "_ueberstunden" = "Arbeitsstunden pro Woche (Durchschnitt)",
      "_sozialer_hintergrund" = "Höchster Bildungsabschluss eines Elternteils",
      "_einkommen_kat" = "Einkommen",
      "_taschengeld_kat" = "Taschengeld",
      "_trinkgeld_kat" = "Trinkgeld",
      "_eink_gesamt" = "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)",
      "_einkommen_systematik" = "Einkommensquellen",
      "_eink_gesamt" = "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)",
      "_kosten_gesamt_kat" = "Gesamtkosten",
      "_notendurchschnitt" = "Notendurchschnitt (DEU, EN, Mathe)",
      "ausbildungsfeld" = "Ausbildungsfeld",
      "schultyp_rec" = "Schultyp",
      "Bundesland" = "Bundesland der Schule",
      "q64_sozialer_hintergrund" = "Familiärer Bildungshintergrund",
      "q38_einkommen_gesamt_kat" = "Einkommen gesamt (kategorisiert)",
      "q38_einkommen_systematik2" = "Einkommensarten",
      "q40_essenskosten_kat" = "Essenkosten (kategorisiert)",
      "q40_fahrtkosten_kat" = "Fahrtkosten (kategorisiert)",
      "q40_arbeitskosten_kat" = "Arbeitskosten (kategorisiert)",
      "q38_taschengeld_kat" = "Taschengeld (kategorisiert)",
      "q38_trinkgeld_kat" = "Trinkgeld (kategorisiert)",
      "q18_anz_bew" = "[Anzahl Bewerbungen] Bewerbungen",
      "q23_unterstuetzung" = "Summenindex für Unterstützung durch Schule (min. = 0; max. = 13)",
      "q51a_SQ001" = "Wie beurteilen Sie den Praktika-Wegweiser insgesamt?",
      "q18_anteil_antworten" = "Anteil Rückmeldungen/Bewerbungen",
      "q18_anz_bew_kat2" = "Anzahl Bewerbungen (kategorisiert)",
      "Bundesland" = "Bundesland Schule",
      "betrieb" = "Größe Betrieb", 
      "bleiben" = "Entsprechung Praktikum - Ausbildungsrichtung", 
      # "schul_neu" = "Schulform_Geschlecht",
      "pläne" = "Pläne nach Ausbildung", 
      "q01_rec" = "Schultyp",
      "q36" = "Unklarheiten im Dienstvertrag",
      "q55" = "Betriebliches Beschäftigungsangebot nach Ausbildung",
      "regional" = "Ort des Praktikums", 
      "richtung_geschlecht" = "Geschlecht - Richtung", 
      "sprache" = "Sprache Zuhause", 
      "schultyp_rec" = "Schulform",
      "sozialer_hg" = "Familiärer Bildungshintergrund",
      "total" = "Gesamt"
    ) %>% 
    mutate(across(matches("ueber"), ~ set_label(.x, "Arbeitsstunden pro Woche (Durchschnitt)"))) 

  saveRDS(pp, "pp_bereinigt.RData")

} else {
  pp <- readRDS("pp_bereinigt.RData")
  }


# Variablenmanagement ----
## Breaks ----
# 
# breaks <- pp.alt %>% select(q1, q1_schulform, q1_schultyp, q4, q24, q27, q28, q29, q31_ueberstunden, q36, q40, q51, q53, q55, q57a, q58_notendurchschnitt, q59_sozialer_hintergrund) %>% names()
# 
# breaks <- lbl.var.df %>% 
#   filter(pp.alt.variable %in% breaks) %>% 
#   select(pp.variable) %>% 
#   unlist %>% 
#   na.omit %>% 
#   as.vector
# 
# saveRDS(breaks,"breaks.RData")
# Numerische Variablen

## Variablen sammeln ----
numeric.vars.1 <- pp %>% 
  select(where(is.numeric)) %>% 
  select(-matches("_n|_total")) %>% 
  names
numeric.vars.2 <- pp %>%
  select(where(~ any(str_detect(.x %>% na.omit,"1 -")))) %>% 
  names

numeric.vars <- c(numeric.vars.1, numeric.vars.2)

# Mehrfachantwortsets
mc.vars <- pp %>% 
  select(where(~ str_detect(.x %>% na.omit,"Nicht Gewählt") %>% any)) %>% 
  names

# Kategoriale = everything() - numeric.vars - mc.vars
categ.vars <- pp %>%
  select(-all_of(c(numeric.vars, mc.vars))) %>% 
  select(-matches("other2_n"), -matches("other$|_n|_total")) %>% 
  names

# Ratingvariablen
rating.varnames <- map(pp, ~ .x %>%
                         get_labels %>% 
                         na.omit %>% 
                         str_detect("(?!.*jährig)^\\d - ")) %>% # "- jährig" kommt in schultypen vor 
  keep(any) %>% 
  names %>% 
  str_remove("(?=\\_).*") %>%
  unique

# breaks
breaks <- c("q02bms","q02bhs", "Bundesland", "ausbildungsfeld", "q24", "q26", "q28", "q29", "q36", "q43", "q55", "q57", "q59", "q61", "q01_rec", "q31", "q64",
            ### neu: 14.7.2026
            "q27_rec",
            NULL)

#### 14.7.2026 überholt mit Codebook aus Standardversion von Dateneinlesen.R 
# # Bibliothek erzeugen
# lbl.var.df <- pp %>% 
#   get_label %>% 
#   named.to.df.fnc(var.col = "pp.variable", label.col = "pp.label")
# 
# lbl.var.df <- lbl.var.df %>% 
#   mutate(var.typ = case_when(
#     pp.variable %in% categ.vars ~ "Kategorisch",
#     pp.variable %in% numeric.vars ~ "Numerisch",
#     pp.variable %in% mc.vars ~ "Mehrfach",
#   )
#   )
# 
# ### Suffixe extrahieren
# lbl.var.df <- lbl.var.df %>% 
#   mutate(
#     # strng.alt = pp.alt.variable %>% str_extract("[a-z]\\d\\d"),
#     strng = pp.variable %>% str_extract("[a-z]\\d\\d")
#   ) 
# 
#
#### 14.7.2026 überholt mit Codebook aus Standardversion von Dateneinlesen.R 
# ### Metainformationen für Variablen
# lbl.var.df <- lbl.var.df %>% 
#   mutate(analytisch = case_when(pp.variable %>% str_detect("q01|q03|q05|id|schultyp$|q14\\d|q35|einkommen_systematik$|other|q08|q12|q18_[1-3]|q31$|q38_[1-3]|q40_[1-3]|q42|q60|q15_[1-2]|q64$|q18_anz_bew|q02bhs|q02bms|_total|_n|cluster_qualitaet_2|cluster_qualitaet_4|Adresse|Bundesland_adresse_rec|Stadt|itemnonr|lastpage|datestamp|seed|startdate|startlanguage|submitdate|gewicht|filter") ~ F, 
#                                 pp.variable %>% str_detect("kosten$|_gesamt$|einkommen$|taschengeld$|trinkgeld$") & var.typ == "Numerisch" ~ F,
#                                 .default = T),
#          breaks = if_else(pp.variable %in% breaks, T, F)) %>% 
#   mutate(ma.kategorie = str_extract(pp.label, "(?<=\\[).+(?=\\])"),
#          Frage = pp.label %>% str_remove("\\[.+\\] "),
#          var.typ = ifelse(pp.variable %>% str_detect("_other$|G11Q60"),
#                           "Offen",
#                           var.typ),
#          # var.typ = ifelse(pp.variable %>% str_detect("q52_"),
#          #                  "Mehrfach",
#          # var.typ),
#          var.typ = ifelse(str_detect(pp.variable,"_n$|_total"),
#                           "Numerisch", 
#                           var.typ),
#          var.typ = ifelse(str_detect(pp.variable,"q18_anteil_antworten"),
#                           "Anteil", 
#                           var.typ))
# 
# ### Händische Korrekturen
# lbl.var.df[which(lbl.var.df$pp.variable == "q41"),"var.typ"] <- "Kategorisch"
# lbl.var.df[which(lbl.var.df$pp.variable == "q31_ueberstunden"),"var.typ"] <- "Kategorisch"
# lbl.var.df[which(lbl.var.df$pp.variable == "q51b"),"var.typ"] <- "Offen"
# lbl.var.df[which(lbl.var.df$pp.variable == "q36a"),"var.typ"] <- "Offen"
# lbl.var.df[which(lbl.var.df$pp.variable == "q58a"),"var.typ"] <- "Offen"
# lbl.var.df[lbl.var.df$strng %in% rating.varnames,"var.typ"] <- "Rating"
# lbl.var.df[which(lbl.var.df$pp.variable == "q58a"),"var.typ"] <- "Offen"
# lbl.var.df[lbl.var.df$pp.variable %in% c("q23_unterstuetzung","q51a_SQ001"),"var.typ"] <- "Rating"
# lbl.var.df[lbl.var.df$pp.variable %in% c("q23_unterstuetzung","q51a_SQ001"),"ma.kategorie"] <- "Mittelwert"
# lbl.var.df <- lbl.var.df %>% 
#   mutate(var.typ = if_else(pp.variable %>% str_detect("q23_\\d+$"),
#                            "Mehrfach", var.typ)) 
# lbl.var.df[lbl.var.df$pp.variable %>%  str_detect("Bundesland$|schultyp_rec|ausbildungsfeld|cluster_qualitaet_3"),"Frage"] <-  c("Bundesland","Schultyp","Ausbildungsfeld","Qualität der Rahmenbedingungen")
# lbl.var.df[lbl.var.df$pp.variable == "q18_anz_bew_kat2", "analytisch"] <- T
# lbl.var.df[lbl.var.df$pp.variable == "q23_unterstuetzung", "analytisch"] <- F
# 
# 
# lbl.var.df <- lbl.var.df %>% 
#   mutate(bericht.analyt = if_else(analytisch == T | str_detect(pp.variable,"_other2"), T, F),
#          var.typ = if_else(pp.variable %>% str_detect("other2"), "Mehrfach", var.typ))
# 
# lbl.var.df <- lbl.var.df %>% 
#   mutate(bericht.analyt = if_else(str_detect(pp.variable,"q14|q35"), F, bericht.analyt))
# 
# 
# brks <- tribble(~pp.variable, ~pp.label, ~var.typ, ~strng, ~analytisch, ~breaks, ~ma.kategorie, ~Frage, ~newvar, ~bericht.analyt,
#                 "total", "Gesamtzeile in TB",                 NA, NA, F, T, NA, "Gesamtzeile in TB", NA, NA,
#                 "richtung_geschlecht", "Richtung Geschlecht", NA, NA, F, T, NA, "Richtung Geschlecht", NA, NA,
#                 "betrieb",     "Größe Betrieb",           NA, NA, F, T, NA, "Größe Betrieb", NA, NA,
#                 "sprache",     "Sprache daheim",          NA, NA, F, T, NA, "Sprache daheim", NA, NA,
#                 "sozialer_hg", "Sozialer Hintergrund",    NA, NA, F, T, NA, "Sozialer Hintergrund", NA, NA,
#                 "regional",    "Praktikum regional",      NA, NA, F, T, NA, "Praktikum regional", NA, NA,
#                 "pläne",       "Pläne nach Ausbildung",   NA, NA, F, T, NA, "Pläne nach Ausbildung", NA, NA,
#                 "bleiben",     "Entsprechung PP-Richtung",NA, NA, F, T, NA, "Entsprechung PP-Richtung", NA, NA
# )
# 
# lbl.var.df <- rbind(lbl.var.df,
#                     brks)
# 
# lbl.var.df[lbl.var.df$pp.variable == "schultyp_rec", c("pp.label","Frage")] <- "Schulform"
# 
# # lbl.var.df <- lbl.var.df %>% # Struktur der Frage entpsricht Mehrfach. Aber viele Weißnicht-Antworten. daher getrennt auswerten
# #   mutate(var.typ = if_else(pp.variable %>% str_detect("q33_\\d+$"),
# #                            "Mehrfach", var.typ)) 
# # lbl.var.df <- lbl.var.df %>% # wird nicht mehr gebraucht, weil keine Duplizierungen wegen alter Variablen vorhanden sind.
# #   mutate(analytisch = if_else(analytisch == T & (slce == 1 | slce %>% is.na), T, F))
# 
# ### Speichern
# saveRDS(lbl.var.df, "Variablenabgleich_alt_neu.RData")
# 
# 
# ## Zuordnung-LS-Gruppen-Fragen ----
# 
# x <- read.csv("Zuordnung-LS-Gruppen-Fragen.csv")
# 
# ord <- x %>%
#   filter(str_length(gruppe) > 0) %>% 
#   separate(gruppe, into = c("gruppe","varnum"), sep = " \\/ ") %>% 
#   rename(strng = variable) %>% 
#   mutate(gruppe = gruppe %>% as.numeric,
#          gruppe = LETTERS[gruppe],
#          newvar = paste0(gruppe,varnum))
# 
# lbl.var.df <- lbl.var.df %>% 
#   left_join(ord[,c("strng","newvar")], by = "strng")
# 
# saveRDS(lbl.var.df, "Variablenabgleich_alt_neu.RData")
# 
# 
# library(openxlsx)
# write.xlsx(lbl.var.df, "Codebuch_pp3.xlsx")
# write.xlsx(lbl.var.df, "../bericht/Codebuch_pp3.xlsx")
# 
# 
# rm(list = setdiff(ls(), ls(pattern = ".fun")))
# "adress.liste|pp|pp_dispens|\\.fun|lbl.var.df|vorlagen.path"



# library(naniar)
# # number of cols without Systemvariables
# ncol(pp_tb)
# 
# # Verteilung der fehlenden Werte
# pp_tb %>%
#   frq(itemnonr)
# 
# ## Visualisierung fehlender Werte
# vis_miss(pp_tb[1:70], warn_large_data = F)
# vis_miss(pp_tb[71:140], warn_large_data = F)
# vis_miss(pp_tb[141:227], warn_large_data = F)
# gg_miss_var(pp_tb) + theme_bw()
# gg_miss_case(pp_tb, show_pct = F) + theme_bw()
# 
# ## Anzahl fehlender Werte pro Frage
# map_pp_tbc(pp_tb, ~ .x %>% is.na %>% sum) %>%
#   pivot_longer(everything()) %>%
#   rename(n_itemnonr = value,
#          variable = name) %>%
#   View
# 
# ## missing
# pp_tb %>% 
#   miss_var_summary() %>% 
#   View
# 
# ## predicting proportion of missingness
# library(rpart)
# library(rpart.plot)
# 
# pp_tb %>% 
#   select(-itemnonr) %>% 
#   add_prop_miss() %>%  # adds a variable with missing proportion per case
#   rpart(prop_miss_all ~ ., 
#         model = T,
#         data = .) %>%
#   rpart.plot(type = 2, 
#              extra = 101,
#              prefix = "Prop. Miss = ")
# 
# ## Anzahl fehlender Wert pro Fall
# pp_tb %>% 
#   select(id,itemnonr) %>% 
#   pivot_longer(-id) %>% 
#   ggplot() +
#   geom_boxplot(aes(x = value, y = value))
# 
# pp_tb %>% 
#   filter(itemnonr < (ncol(.)/2))



# Tabellenband Bericht ----
### 14.7.2026: Die Änderung von pp auf pp_tb würde ich nicht beibehalten sondern nur in pp weiterarbeiten
pp_tb <- pp
x <- var.finder.label.fnc(pp_tb, "Geschlecht")$variable
y <- var.finder.label.fnc(pp_tb, "dichotom")$variable

pp_tb <- pp_tb %>%
  mutate(schul_neu = fct_cross(x, y, sep = " - "))     


# alt.var <- c("q1","q55","q28","q57a","q59_sozialer_hintergrund","q24","q53","q54","q1_schultyp")
# x <- pp2 %>% 
#   select(all_of(alt.var)) %>% 
#   get_label %>% 
#   named.to.df.fnc()
# y <- map2(x$label, x$name, ~ var.finder.label.fnc(pp,.x) %>% 
#             mutate(alt = .y))
# x
# y
# pp2$q28
# var.finder.label.fnc(pp,"Pläne")
# pp %>% 
#   select(matches("sozialer_")) %>% 
#   get_label
# # token.search.fnc(lbl.var.df$pp.label, x$label)
#
# neu.var <- c("schultyp_rec",
#              "q59",
#              "q28",
#              "q57",
#              "q64_sozialer_hintergrund",
#              "q24",
#              "q61",
#              "q58",
#              "q01_rec",
#              "schul_neu")
# neu.var <- c(neu.var, "schul_neu")
#
# ## 14.6.2026: mit standardisierten Codebooks wird die Verwendung separater Libraries obsolet.
# ## Mit diesem DF kann ich auf Variablennamen (und deren Änderungen) dynmisch reagieren.
# variables <- pp_tb[which(names(pp_tb) %in% neu.var)] %>%
#   get_label %>% 
#   named.to.df.fnc() %>% 
#   mutate(gen.var = c("regional",
#                      "betrieb",
#                      "pläne",
#                      "bleiben",
#                      "geschlecht",
#                      "sprache",
#                      "q01_rec",
#                      "schultyp_rec",
#                      "sozialer_hg",
#                      "schul_neu"
#   )
#   )
# 
# ## Es geschehen immer kleine Änderungen im Wording. Das aktuelle Wording in einer Liste abzuspeichern verhindert fehlerhafte Zuordnungen
# # lbs mit variables verknüpfen
# lbs <- pp_tb[neu.var] %>% 
#   get_labels
# saveRDS(lbs, "pp_tb_neue_labels.RData")
# x <- variables %>% 
#   arrange(match(name, names(lbs))) %>% 
#   select(gen.var) %>% 
#   unlist
# names(lbs) <- x
# x <- lbs %>% 
#   unlist %>% 
#   named.to.df.fnc(var.col = "gen.var", label.col = "kategorie") %>% 
#   mutate(gen.var = str_remove(gen.var, "\\d+$"))
# variables.df <- variables %>% 
#   left_join(x, by = "gen.var")
# # variables
#
# pp_tb <- pp_tb %>%
#   # frq(schultyp_rec)
#   # mutate(!!sym(variables[10,1] %>% unlist) := fct_recode(!!sym(variables[10,1] %>% unlist),
#   mutate(!!sym(variables.df %>% filter(gen.var == "schultyp_rec") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "schultyp_rec") %>% select(name) %>% unique %>% unlist), 
#                                                                                                                            "BHS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAK"),
#                                                                                                                            "BMS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAS"),
#                                                                                                                            "BHS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("HTL"),
#                                                                                                                            "BMS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("techn.+Fachschule"),
#                                                                                                                            "BHS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("HLW"),
#                                                                                                                            "BMS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("Fachschule.+wirtschaft"),
#                                                                                                                            "BHS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("HLT"),
#                                                                                                                            "BMS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("Hotelfachschule"),
#                                                                                                                            "BHS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("Höhere.+ädag"),
#                                                                                                                            # "BMS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("BMS.+ädag"),
#                                                                                                                            # "BHS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("BHS.+Sozial"),
#                                                                                                                            "BMS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("Sozialberuf"),
#                                                                                                                            NULL = "Dreijährige Fachschule für Pädagogische Assistenz") %>% 
#            fct_relevel("BHS (kaufm.)",
#                        "BMS (kaufm.)",
#                        "BHS (techn.)",
#                        "BMS (techn.)",
#                        "BHS (wirtschaft.)",
#                        "BMS (wirtschaft.)",
#                        "BHS (tourist.)",
#                        "BMS (tourist.)",
#                        "BHS (pädag.)",
#                        # "BMS (pädag.)",
#                        # "BHS (sozial)",
#                        "BMS (sozial)"),
#          !!sym(variables.df %>% filter(gen.var == "regional") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "regional") %>% select(name) %>% unique %>% unlist),
#                                                                                                                        "Regional"     = lbs[["regional"]][1],
#                                                                                                                        "Überregional" = lbs[["regional"]][2],
#                                                                                                                        "Im Ausland"   = lbs[["regional"]][3]),
#          !!sym(variables.df %>% filter(gen.var == "betrieb") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "betrieb") %>% select(name) %>% unique %>% unlist),
#                                                                                                                       "1-4 MA"      = lbs[["betrieb"]][1],
#                                                                                                                       "5-9 MA"      = lbs[["betrieb"]][2],
#                                                                                                                       "10-49 MA"    = lbs[["betrieb"]][3],
#                                                                                                                       "50-99 MA"    = lbs[["betrieb"]][4],
#                                                                                                                       "100-250 MA"  = lbs[["betrieb"]][5],
#                                                                                                                       "Über 250 MA" = lbs[["betrieb"]][6]),
#          !!sym(variables.df %>% filter(gen.var == "pläne") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "pläne") %>% select(name) %>% unique %>% unlist),  
#                                                                                                                     "Suche nach Arbeitsplatz"   = lbs[["pläne"]][1], 
#                                                                                                                     "Weiterführende Ausbildung" = lbs[["pläne"]][2], 
#                                                                                                                     "Weiß noch nicht genau"     = lbs[["pläne"]][3]),
#          !!sym(variables.df %>% filter(gen.var == "bleiben") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "bleiben") %>% select(name) %>% unique %>% unlist), 
#                                                                                                                       "Ja"                               = lbs[["bleiben"]][1],
#                                                                                                                       "Mit Ausbildungsrichtung verwandt" = lbs[["bleiben"]][2],
#                                                                                                                       "Nein"                             = lbs[["bleiben"]][3]),
#          !!sym(variables.df %>% filter(gen.var == "geschlecht") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "geschlecht") %>% select(name) %>% unique %>% unlist),                                                  NULL = "Divers"),
#          richtung_geschlecht = fct_cross(!!sym(variables.df %>% filter(gen.var == "geschlecht") %>% select(gen.var) %>% unique %>% unlist),
#                                          ausbildungsfeld, sep = " - "),
#          richtung_geschlecht = fct_recode(richtung_geschlecht,
#                                           "Männlich - Pädagogisch"    = 'Männlich - Bildungsanstalt pädagogisch',
#                                           "Weiblich - Pädagogisch"    = 'Weiblich - Bildungsanstalt pädagogisch',
#                                           # "Männlich - Humanberuflich"  = "Männlich - Humanberufliche Schule",
#                                           # "Weiblich - Humanberuflich"  = "Weiblich - Humanberufliche Schule",
#                                           "Männlich - Kaufmännisch"    = "Männlich - Kaufmännische Schule",
#                                           "Weiblich - Kaufmännisch"    = "Weiblich - Kaufmännische Schule",
#                                           "Männlich - Wirtschaftlich"    = "Männlich - Wirtschaftliche Schule",
#                                           "Weiblich - Wirtschaftlich"    = "Weiblich - Wirtschaftliche Schule",
#                                           "Männlich - Techn.-gewerbl." = "Männlich - Technisch gewerbliche Schule",
#                                           "Weiblich - Techn.-gewerbl." = "Weiblich - Technisch gewerbliche Schule"),
#          !!sym(variables.df %>% filter(gen.var == "sprache") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "sprache") %>% select(name) %>% unique %>% unlist),  
#                                                                                                                       "Deutsch und andere Sprache(n)"     = lbs[["sprache"]][2],
#                                                                                                                       "Andere Sprache(n) und Deutsch"     = lbs[["sprache"]][3],
#                                                                                                                       "Nur andere Sprache(n) als Deutsch" = lbs[["sprache"]][4]),
#          !!sym(variables.df %>% filter(gen.var == "sozialer_hg") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "sozialer_hg") %>% select(name) %>% unique %>% unlist),
#                                                                                                                           "Niedrig" = lbs[["sozialer_hg"]][1], 
#                                                                                                                           "Mittel"  = lbs[["sozialer_hg"]][2],
#                                                                                                                           "Gehoben" = lbs[["sozialer_hg"]][3],
#                                                                                                                           "Hoch"    = lbs[["sozialer_hg"]][4]),
#          total = "-")
# 
# ## Branchen als Break inkludieren: levels mit n<35 löschen --> Person erhält NA
# pp_tb <- pp_tb %>%
#   mutate(q27_alt = q27) 
# ex.branche <- pp_tb %>%
#   count(q27_alt, wt=gewicht) %>% 
#   filter(n < 35) %>% 
#   select(q27_alt) %>% 
#   unlist %>% 
#   as.vector
# 
# pp_tb <- pp_tb %>%
#   mutate(q27 = case_match(q27_alt, ex.branche ~ "Sonstige",
#                           .default = q27_alt) %>% 
#            fct_relevel("Sonstige", after = 14))
# 
# pp_tb$q27 <- set_label(pp_tb$q27, 
#                        "In welchem Berufsbereich haben Sie Ihr Praktikum absolviert?")
# 
# ## Level von Geschlecht-Richtung
# pp_tb <- pp_tb %>% 
#   mutate(richtung_geschlecht = fct_relevel(richtung_geschlecht, c("Männlich - Kaufmännisch",
#                                                                   "Weiblich - Kaufmännisch",
#                                                                   "Männlich - Techn.-gewerbl.",
#                                                                   "Weiblich - Techn.-gewerbl.",
#                                                                   "Männlich - Wirtschaftlich",
#                                                                   "Weiblich - Wirtschaftlich",
#                                                                   "Männlich - Tourismus",
#                                                                   "Weiblich - Tourismus",
#                                                                   "Männlich - Pädagogisch",
#                                                                   "Weiblich - Pädagogisch",
#                                                                   "Männlich - Soziales",
#                                                                   "Weiblich - Soziales")))
# alt.var <- c("q51","q36","q4","q1_schultyp")
# x <- pp2 %>% 
#   select(all_of(alt.var)) %>% 
#   get_label %>% 
#   named.to.df.fnc()
# y <- map2(x$label, x$name, ~ var.finder.label.fnc(pp,.x) %>% 
#             mutate(alt = .y))
# x
# y
# pp2$q28
# var.finder.label.fnc(pp,"Pläne")
# pp %>% 
#   select(matches("sozialer_")) %>% 
#   get_label


breaks_tab <- pp_tb %>% select(all_of(c("total",
                                        "q01_rec",
                                        "schultyp_rec",
                                        "richtung_geschlecht",
                                        "Bundesland",
                                        "betrieb",
                                        "sprache",
                                        "sozialer_hg",
                                        "regional",
                                        "pläne",
                                        "bleiben",
                                        "q36",
                                        "q31_ueberstunden",
                                        "q55",
                                        "pläne",
                                        "cluster_qualitaet_3",
                                        "q27"))) %>% names()

pp_tb <- pp_tb %>%
  left_join(pp %>% select(schultyp_rec, id) %>% rename(schultyp_rec.pp = schultyp_rec), by = "id") 

pp_tb <- pp_tb %>%
  select(-gewicht) %>%
  left_join(gewicht %>% rename(schultyp_rec.pp = schultyp_rec), by = c("schultyp_rec.pp","Bundesland"))

saveRDS(pp_tb, "pp_tb.RData")
saveRDS(breaks_tab, "breaks_tab.RData")


## Univariat - PP ----

### Da sämtliche nicht-numerischen Variablen (die Funktion "tabellenband_univariat" ist nur auf solche ausgelegt) ausgewertet werden sollten (mit Ausnahmen wie Adressen), überspringe ich das Aufrufen einzelner Variablen wie in der vorangegangenen Syntax von Martin und suche gleich nach nicht-numerischen Variablen und schließe ggf. einzelne Variablen aus.

lbl.var.df <- readRDS("Variablenabgleich_alt_neu.RData")

univariat.varnames <- lbl.var.df %>% 
  filter(analytisch == T & var.typ != "Anteil" & (pp.variable %>% str_detect("_other|q58a|q51b|G11Q60|q23_unterstuetzung", negate = T))) %>% 
  select(pp.variable) %>% 
  unlist %>% 
  as.vector

# pp_tb[univariat.varnames[181]]

# univariat.varnames <- pp %>% 
#   select(where(~ !is.numeric(.x))) %>% # alle nicht-numerischen Variablen auswählen
#   select(!matches("_n|_total")) %>% # die neuberechneten Mehrfachauswahlvariablen ausschließen
#   select(-c(Stadt,Adresse,schultyp_rec, Bundesland_adresse_rec)) %>% 
#   names
# 
# univariat.varnames <- univariat.varnames[which(univariat.varnames %>% str_detect("q05", negate = T))] # Adressvariablen ausschließen

tab_uni <- tabellenband_univariat(pp_tb, univariat.varnames, weighted = T, weight = gewicht) %>% list() %>% set_names("univariat")

tab_uni <- set_names(tab_uni,"univariat")
saveRDS(tab_uni, "tab_uni.RData")

tab_uni <- 
tab_uni %>% 
  .[[1]] %>% 
  mutate(across(c(Variable, Fragestellung, Fragekategorien), ~ if_else(.x == "-", NA, .x))) %>% 
  fill(Variable, Fragestellung, Fragekategorien)

wb <- createWorkbook()
addWorksheet(wb, "univariat")
writeDataTable(wb,"univariat", tab_uni, tableStyle = "TableStyleMedium2")#,startCol = which(colnames(df)=="URL"), startRow = 2)
saveWorkbook(wb, "pp_tb_univariat.xlsx", overwrite = TRUE)
shell.exec("pp_tb_univariat.xlsx")



# Exportieren ----

## Bereinigter DF ----
pp_dispens <- pp_dispens %>% 
  mutate(across(matches("q40"), ~ .x %>% as.numeric),
         across(matches("q42"), ~ .x %>% as.numeric))

pp_joint <- pp %>% 
  # select(q40_1) %>% frq
  full_join(pp_dispens) 

# pp_joint <- pp_joint %>%
#   select(-gewicht) %>%
#   left_join(gewicht, by = c("schultyp_rec","Bundesland"))

saveRDS(pp_joint, "pp_bereinigt_vereinigt.RData")
write_spss(pp_joint, "../datensatz/pp_bereinigt_vereinigt.sav")
write.xlsx(pp_joint, "../datensatz/pp_bereinigt_vereinigt.xlsx")


## PP Origin ----
pp_origin <- readRDS("pp3.RData")

pp_origin <- pp_origin %>% 
  mutate(schultyp_rec = case_when(
    q02bhs %>% str_detect("Elementarpädagogik") ~ paste0("Höhere ",q02bhs),
    q02bhs %>% str_detect("Kaufmän") ~ "Handelsakademie (HAK)",
    q02bhs %>% str_detect("HTL|Mode") ~ "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
    q02bhs %>% str_detect("HLW") ~ "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
    q02bhs %>% str_detect("Tourism") ~ "Höhere Lehranstalt für Tourismus (HLT)",
    q02bms %>% str_detect("Handelsschule") ~ "Handelsschule (HAS)",
    q02bms %>% str_detect("Fachschule für wirtschaftliche Berufe") ~ "Fachschule für wirtschaftliche Berufe",
    q02bms %>% str_detect("Hotel|Tourism") ~ "Hotelfachschule, Tourismusfachschule",
    q02bms %>% str_detect("Sozial") ~ "Dreijährige Fachschule für Sozialberufe",
    q02bms %>% str_detect("Pädag") ~ "Dreijährige Fachschule für Pädagogische Assistenz", # wurden herausgefiltert
    is.na(q02bms) & is.na(q02bhs) ~ NA,
    q02bms %>% str_detect(paste0(c("Handelsschule", # das, was nach den vorherigen BMS übrigbleibt
                                   "wirtschaftliche Berufe",
                                   "Hotel",
                                   "Tourism",
                                   "Sozial",
                                   "[P-p]ädag"), collapse = "|"), negate = T) ~ "Gewerbliche, technische und kunstgew. Fachschulen"))

### Levels verändern
pp_origin <- pp_origin %>% 
  # select(schultyp_rec) %>% unique
  mutate(schultyp_rec = factor(schultyp_rec, levels = c("Handelsakademie (HAK)",
                                                        "Handelsschule (HAS)",
                                                        "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
                                                        "Gewerbliche, technische und kunstgew. Fachschulen",
                                                        "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
                                                        "Fachschule für wirtschaftliche Berufe",
                                                        "Höhere Lehranstalt für Tourismus (HLT)",
                                                        "Hotelfachschule, Tourismusfachschule",
                                                        "Höhere Bildungsanstalt für Elementarpädagogik oder Sozialpädagogik",
                                                        # "Dreijährige Fachschule für Pädagogische Assistenz",
                                                        "Dreijährige Fachschule für Sozialberufe"))) 
# pp_origin$schultyp_rec <- pp_origin$schultyp_rec %>% droplevels

pp_origin <- pp_origin %>% # Gewicht anhängen
  select(-gewicht) %>%
  left_join(gewicht, by = c("schultyp_rec","Bundesland"))

lbs <- readRDS("pp_tb_neue_labels.RData")
pp_origin <- pp_origin %>% 
  mutate(schultyp_rec.pp = schultyp_rec, 
         schultyp_rec = fct_recode(schultyp_rec,
                                   "BHS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAK"),
                                   "BMS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAS"),
                                   "BHS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("HTL"),
                                   "BMS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("techn.+Fachschule"),
                                   "BHS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("HLW"),
                                   "BMS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("Fachschule.+wirtschaft"),
                                   "BHS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("HLT"),
                                   "BMS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("Hotelfachschule"),
                                   "BHS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("Höhere.+ädag"),
                                   # "BMS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("BMS.+ädag"),
                                   # "BHS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("BHS.+Sozial"),
                                   "BMS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("Sozialberuf"),
                                   NULL = "Dreijährige Fachschule für Pädagogische Assistenz") %>% 
           fct_relevel("BHS (kaufm.)",
                       "BMS (kaufm.)",
                       "BHS (techn.)",
                       "BMS (techn.)",
                       "BHS (wirtschaft.)",
                       "BMS (wirtschaft.)",
                       "BHS (tourist.)",
                       "BMS (tourist.)",
                       "BHS (pädag.)",
                       # "BMS (pädag.)",
                       # "BHS (sozial)",
                       "BMS (sozial)"),
         total = "-")

pp_origin$schultyp_rec <- set_label(pp_origin$schultyp_rec,"Schulform")
pp_origin$Bundesland <- set_label(pp_origin$Bundesland,"Bundesland")
pp_origin$total <- set_label(pp_origin$total,"Gesamt")

breaks <- c("total","Bundesland","schultyp_rec")
tab_bi_origin <- tbl.band.fnc(pp_origin, var = "q08", brk = all_of(breaks), weighted = T, weight = "gewicht")
tab_bi_origin <- set_names(tab_bi_origin,"bivariat_origin")
saveRDS(tab_bi_origin, "tab_bi_origin.RData")
write.xlsx(tab_bi_origin, "../bericht/Tabellen/pp_status_tabelle.xlsx", overwrite = T, startRow = 1)


## Dispens ----
load("pp2.RData")
pp_dispens <- readRDS("pp_dispens.RData")
lbs <- readRDS("pp_tb_neue_labels.RData")

pp_dispens <- pp_dispens %>%
  # frq(schultyp_rec)
  # mutate(!!sym(variables[10,1] %>% unlist) := fct_recode(!!sym(variables[10,1] %>% unlist),
  mutate(!!sym(variables.df %>% filter(gen.var == "schultyp_rec") %>% select(gen.var) %>% unique %>% unlist) := fct_recode(!!sym(variables.df %>% filter(gen.var == "schultyp_rec") %>% select(name) %>% unique %>% unlist), 
                                                                                                                           "BHS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAK"),
                                                                                                                           "BMS (kaufm.)"      = lbs[["schultyp_rec"]] %>% str_subset("HAS"),
                                                                                                                           "BHS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("HTL"),
                                                                                                                           "BMS (techn.)"      = lbs[["schultyp_rec"]] %>% str_subset("techn.+Fachschule"),
                                                                                                                           "BHS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("HLW"),
                                                                                                                           "BMS (wirtschaft.)" = lbs[["schultyp_rec"]] %>% str_subset("Fachschule.+wirtschaft"),
                                                                                                                           "BHS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("HLT"),
                                                                                                                           "BMS (tourist.)"    = lbs[["schultyp_rec"]] %>% str_subset("Hotelfachschule"),
                                                                                                                           "BHS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("Höhere.+ädag"),
                                                                                                                           # "BMS (pädag.)"      = lbs[["schultyp_rec"]] %>% str_subset("BMS.+ädag"),
                                                                                                                           # "BHS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("BHS.+Sozial"),
                                                                                                                           "BMS (sozial)"      = lbs[["schultyp_rec"]] %>% str_subset("Sozialberuf"),
                                                                                                                           NULL = "Dreijährige Fachschule für Pädagogische Assistenz") %>% 
           fct_relevel("BHS (kaufm.)",
                       "BMS (kaufm.)",
                       "BHS (techn.)",
                       "BMS (techn.)",
                       "BHS (wirtschaft.)",
                       "BMS (wirtschaft.)",
                       "BHS (tourist.)",
                       "BMS (tourist.)",
                       "BHS (pädag.)",
                       # "BMS (pädag.)",
                       # "BHS (sozial)",
                       "BMS (sozial)"))


x <- pp2 %>%
  select(q1, q4, q6, q7, q8, q8a, q9_1:q9_7, q53:q55, q57a, q58_1:q58_3, q59) %>% # q9_other2, q10_1_kat,
  get_label() %>% 
  named.to.df.fnc()


vars_univariat_dispens <- lbl.var.df %>%
  filter(pp.label %in% c(x$label %>% unlist) | pp.variable %in% c("q12","q12a","q13_1","q13_2","q13_3","q13_4","Bundesland","schultyp_rec")) %>%
  select(pp.variable) %>%
  unlist %>%
  as.vector %>%
  unique 

pp_dispens$q12 %>% frq

vars_univariat_dispens_1 <- pp_dispens %>%
  select(all_of(vars_univariat_dispens), q15_1_kat) %>%
  select(!matches("_n|_total|Bundesland_adresse_rec")) %>%
  names

tab_uni_dispens <- tabellenband_univariat(pp_dispens, vars_univariat_dispens_1, weighted = F) %>% list() %>% set_names("univariat_dispens")
saveRDS(tab_uni_dispens, "tab_uni_dispens.RData")
write.xlsx(tab_uni_dispens, "../bericht/Tabellen/pp_uni_dispens.xlsx", overwrite = T, startRow = 1)



breaks <- lbl.var.df %>% 
  filter(pp.variable %in% c("Bundesland","schultyp_rec")) %>% 
  select(pp.variable) %>% 
  unlist %>% 
  as.vector
vars_univariat_dispens_2 <- pp_dispens %>%
  select(all_of(vars_univariat_dispens), q15_1_kat) %>%
  select(-c(Bundesland,schultyp_rec)) %>% 
  select(!matches("_n|_total|Bundesland_adresse_rec")) %>%
  names

tab_bi_dispens <- tabellenband_bivariat(pp_dispens, all_of(breaks), col_vars = all_of(vars_univariat_dispens_2), weight = gewicht, weighted = T, show_n_weighted = T)
tab_bi_dispens <- set_names(tab_bi_dispens,vars_univariat_dispens_2)

saveRDS(tab_bi_dispens, "tab_bi_dispens.RData")
write.xlsx(tab_bi_dispens, "../bericht/Tabellen/pp_bi_dispens.xlsx", overwrite = T, startRow = 1)


## Einkommen ----
pp_tb %>% 
  group_by(schultyp_rec) %>%
  reframe(m = median(q38_eink_gesamt, na.rm = T),
          n = n()) %>% 
  mutate(newcat = paste0(schultyp_rec, " (n=", c_across(last_col()),")"),.before = 1) %>% 
  select(-schultyp_rec)

pp_tb %>% 
  reframe(m = median(q38_eink_gesamt, na.rm = T),
          n = n()) %>% 
  mutate(newcat = paste0(schultyp_rec, " (n=", c_across(last_col()),")"),.before = 1)

pp_tb %>% 
  mutate(filter = case_when(str_detect(schultyp_rec,"ädag") & q38_eink_gesamt != 0 ~ "m.EK",
                            str_detect(schultyp_rec,"ädag") & q38_eink_gesamt == 0 ~ "o.EK",
                            str_detect(schultyp_rec,"ädag") & q38_eink_gesamt %>% is.na ~ "k.A.")) %>% 
  freq(filter)
group_by(filter) %>% 
  reframe(n = n())
frq(q38_eink_gesamt)



# Tabellenband als Excel ----


# tbl.kat <- readRDS("tbl_kategorisch.RData")
# tbl.ant <- readRDS("tbl_anteil.RData")
# tbl.rati <- readRDS("tbl_rating.RData")
# tbl.mehr <- readRDS("tbl_mehrfach.RData")

tbl.list <- readRDS("tbl_2gether.RData") # stammt uas Tabellenband.Rmd und enthält alle Tabellen
tbl.list <- map(tbl.list, ~ .x %>% mutate(across(where(is.numeric), ~ round(.x, 2))))
library(openxlsx)
write.xlsx(tbl.list, "../bericht/Tabellenband/pp_tabellenband.xlsx", overwrite = T, startRow = 1)


# Originalen Tabellenband nachbauen
tbl.grafiken <- map(tbl.list.2gether, ~ .x %>% 
                      mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
                      rowwise %>%
                      mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
                             newcat = str_replace(newcat, "\\) \\(", "\\, "),
                             newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
                             .before = 3) %>% 
                      mutate(across(is.numeric, ~ .x %>% round(0))) %>% 
                      ungroup)
saveRDS(tbl.grafiken, "pp_tabellenband.RData")
write.xlsx(tbl.grafiken, "../bericht/Tabellen/pp_tabellenband.xlsx")




# Nachträglich für Bericht ----

# Vergleiche und spezielle Auswertungen für den Bericht sollten nach Möglichkeit nur aus dem Tabellenband entnommen werden. Entsprechende Syntax kann hier eingeführt werden. Dies ist insbesondere von herausragender Bedeutung, damit die Zahlen immer übereinstimmen.


## Tabellen Grafiken ----

### 2024 ----

# VORSICHT: Syntax fehlerhaft!!!!
### Stichprobenbeschreibung
# stchprb <- tbl.band.fnc(pp_tb, var = c("q59","q61","q64"), brk = c("total","schultyp_rec"), weighted = T, weight = "gewicht")
# names(stchprb) <- c("q59","q61","q64")
# 
# stchprb %>% 
#   write.xlsx(file = "../bericht/Tabellen/Stichprobenbeschreibung.xlsx")
# 
# ### Dispens  
# pp_dispens$q15_1 %>% frq
# 
#   
# ## Kategorie: Sonstige für Bericht ausgeben
# lbs.mehr.1 <- lbl.var.df %>%
#     filter(bericht.analyt == T & var.typ %>% str_detect("Mehrfach")) %>%
#     group_by(strng) %>%
#     select(strng, pp.variable, Frage, ma.kategorie, newvar) %>% 
#     mutate(string = if_else(is.na(newvar) == T, strng, newvar),
#            length = str_count(ma.kategorie)) %>% 
#     arrange(strng) %>% 
#     nest %>% 
#     ungroup %>%
#     select(-strng) %>%
#     reduce(~ .x)
# mehr.vars.1 <- map(lbs.mehr.1, ~.x[,1] %>% unlist %>% str_extract(".+_other2") %>% na.omit %>% as.vector) %>% 
#   compact %>% 
#   unlist
# 
# suppressWarnings({
#     out <- map(mehr.vars.1, ~ pp %>%
#                  questionr::tabs(.x, breaks_tab[3], weight = "gewicht") %>% 
#                  as.data.frame() %>% 
#                  rownames_to_column("kat") %>% 
#                  as_tibble %>%
#                  mutate_all(~ as.character(.x)) %>% 
#                  pivot_longer(-1, names_to = "break_tab", values_to = "Sonstige") %>% 
#                  select(2,1,3) %>% 
#                  filter(.[,2] %>% is.na == F) %>% 
#                  arrange(break_tab) %>% 
#                  filter(.[,2] == "Ja") %>% 
#                  rbind(NA) %>% 
#                  slice(1:10,12,11) %>% 
#                  select(-kat) %>% 
#                  mutate(Sonstige = format(Sonstige %>% as.numeric %>% round(0), 
#                                           decimal.mark = ",", big.mark = "."),
#                              across(everything(), ~ if_else(.x == "NA" | .x %>% is.na, "", .x)),
#                              break_tab = if_else(break_tab == "Overall", "Gesamt", break_tab))
#                     )
# })
# names(out) <- mehr.vars.1
# saveRDS(out, "tbl_mehrfach_sonstige.RData")
# write.xlsx(out, "../bericht/Tabellen/pp_grafiken_schulform_sonstige.xlsx")
# # system("open ../bericht/Tabellen/pp_grafiken_schulform_sonstige.xlsx")
# 
# # pp %>% 
# #   select(matches("total")) %>% 
# #   reframe(across(everything(), ~ if_else(.x == 0, 0, 1) %>% sum(na.rm = T))) %>% 
# #   pivot_longer(everything(), values_to = "n bei Mehrfach") %>% 
# #   full_join(pp %>% 
# #           select(matches("total")) %>% 
# #           reframe(across(everything(), ~ if_else(.x == 0, NA, .x) %>% mean(na.rm = T))) %>% 
# #           pivot_longer(everything(), values_to = "Durchschn. Mehrfachzahl"), by = "name")
# 
# 
# 
# ### Für den Bericht sollen die Rating-Fragen als kategoriale Variablen ausgewertet werden
# tbl.list.2gether <- readRDS("tbl_2gether.RData")
# tbl.list <- readRDS("tbl_list.RData")
# 
# lbs.rati <- ft.label.fnc(vart = "Rating")
# lbs.rati.1 <- lbs.rati %>% 
#   list_rbind() %>% 
#   mutate(strng = str_extract(pp.variable, "[a-z]\\d+.*(?=_)"))
# 
# rati.vars <- lbl.var.df %>%
#   filter(analytisch == T & var.typ == "Rating") %>% 
#   group_by(strng) %>%
#   reframe(var = list(pp.variable)) %>% 
#   select(-strng) %>% 
#   reduce(~ .x)
# rati.vars.1 <- rati.vars %>% unlist
# 
# tbl.rati.1 <- list()
# for (i in 1:length(rati.vars.1)) {
#   tbl.rati.1[[i]] <- tbl.band.fnc(pp_tb, var = rati.vars.1[i], brk = breaks_tab, weighted = T, weight = "gewicht")
# }
# tbl.rati.1 <- map(tbl.rati.1, ~.x[[1]])
# names(tbl.rati.1) <- rati.vars.1
# saveRDS(tbl.rati.1, "tbl.rati.1.RData")
# # tbl.list.2gether.1 <- tbl.list.2gether[-which(names(tbl.list.2gether) %in% names(tbl.rati.1))] # alten Rati-Variablen entfernen
# tbl.list.2gether.1 <- c(tbl.list.2gether,tbl.rati.1) # neue hinzuf?gen
# ordered <- names(tbl.list.2gether.1) %>% # objekt mit sortierter Reihenfolge erstellen
#   named.to.df.fnc() %>% 
#   mutate(char = str_extract(label,"^[A-Z a-z]"),
#          num1 = str_extract(label, "(?<=^[A-Z a-z])\\d+") %>% as.integer,
#          num2 = str_extract(label, "(?<=_).+") %>% 
#           str_extract("\\d+")  %>%
#            as.integer,
#          char = if_else(num1 %>% is.na, NA, char)) %>% 
#   arrange(char,num1,num2) %>% 
#   select(-name)
# tbl.list.2gether.1 <- tbl.list.2gether.1[ordered$label] # Reihenfolge wiederherstellen
# tbl.grafiken.1 <- map2(tbl.list.2gether.1, names(tbl.list.2gether.1), ~ .x %>% 
#                         mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#                         rowwise %>%
#                         mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"), # Kategorie mit stichprobengr??e
#                                newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                                newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                                .before = 3) %>% 
#                         mutate(across(is.numeric, ~ .x %>% round(0)),
#                                Kategorie = str_remove(Kategorie, "\\)")) %>% 
#                         ungroup %>% 
#                         tidyr::fill(Fragestellung, .direction = "down") %>% # Nur Zeilen mit Schulform und Gesamt ?bernehmen und umbauen f?r effizientere Verwendung bei Erstellung der Grafiken
#                         filter(Fragestellung %>% str_detect("Gesamt|Schulform")) %>% 
#                         add_row() %>% 
#                         slice(which(.$Fragestellung == "Schulform"),
#                               which(.$Fragestellung %>% is.na),
#                               which(.$Fragestellung == "Gesamt")) %>% 
#                         select(-c(1), -matches("Gesamt")) %>% 
#                         relocate(1, .after = ncol(.)) %>% 
#                         rename(!!sym(paste0(.y,".")) := 1)) # Variablennamen einfügen, um nachträglich besser zu finden
# saveRDS(tbl.grafiken.1, "pp_grafiken_schulform.RData")
# write.xlsx(tbl.grafiken.1, "../bericht/Tabellen/pp_grafiken_schulform.xlsx")
# 
# 
# ### Branchen als Break
# pp_tb %>% 
#   count(q27, wt = gewicht) %>% 
#   na.omit %>% 
#   mutate(p = n/5904,
#          s = sum(p))# %>% 
#   
# tbl.band.fnc(pp_tb, var = "schultyp_rec", brk = c("total","q27"), weighted = T, weight = "gewicht") %>% 
#   # flatten %>% 
#   write.xlsx("../bericht/Tabellen/tlb_branche_schultyp.xlsx")
# system("open ../bericht/Tabellen/tlb_branche_schultyp.xlsx")
#   
#   
# a <- c("q37",
#        "q31_ueberstunden",
#        "q32",
#        "q53_2",
#        "q55",
#        "q56",
#        "cluster_qualitaet_3",
#        "q40_kosten_gesamt_kat")
# 
# branchen <- list()
# for (i in 1:length(a)) {
#   branchen[[i]] <- tbl.band.fnc(pp_tb, var = a[i], brk = c("total","q27"), weighted = T, weight = "gewicht")
# }
# b <- map(branchen, ~ .x[[1]])
# branchen.1 <- map(b, ~ .x %>% 
#                       mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#                       rowwise %>%
#                       mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
#                              newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                              newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                              .before = 3) %>% 
#                       mutate(across(is.numeric, ~ .x %>% round(0))) %>% 
#                       ungroup %>% 
#                     add_row() %>% 
#                     slice(which(.$Fragestellung == "Schulform"),
#                                                       which(.$Fragestellung %>% is.na),
#                                                       which(.$Fragestellung == "Gesamt")) %>% 
#                     select(-c(1), -matches("Gesamt")) %>% 
#                     relocate(1, .after = ncol(.)) %>% 
#                     rename(q56. := 1))
# names(branchen.1) <- a
# # q56 benötigt zusäztliche Filterungen
# # branchen.1$q56 %>% # damit werden Branchen mit n > 35 behalten
# #   filter((.[[ncol(.)-1]] %>% as.numeric) > 35) %>% 
# #   select(1) %>% 
# #   unlist %>% 
# #   str_extract("^.{5}") %>% cat(sep = '|')
# q56 <- tbl.band.fnc(pp_tb %>% 
#                       filter(as.integer(q57) == 1 & 
#                                as.integer(q55) == 1 &
#                                str_detect(q27, "Büro,|Elekt|Hande|Masch|Pädag|Touri")), 
#                     var = "q56", brk = c("total","q27"), weighted = T, weight = "gewicht") %>%
#   flatten %>% 
#   as_tibble %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#   rowwise %>%
#   mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
#                     newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                     newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                     .before = 3) %>% 
#   mutate(across(is.numeric, ~ .x %>% round(1))) %>% 
#   ungroup %>% 
#   add_row() %>% 
#   slice(which(.$Fragestellung == "Schulform"),
#                       which(.$Fragestellung %>% is.na),
#                       which(.$Fragestellung == "Gesamt")) %>% 
#   select(-c(1), -matches("Gesamt")) %>% 
#   relocate(1, .after = ncol(.)) %>% 
#   rename(q56. := 1) 
# branchen.1$q56 <- q56 # Listelement ersetzen
# ## Branchen > 35 herausfiltern
# branchen.2 <- map(branchen.1, ~.x %>%
#       mutate(across(ncol(.)-1, ~ .x %>% str_remove("\\.") %>% as.numeric),
#          across(ncol(.)-1, ~ case_when(.[[1]] == "Gesamt" ~ sum(.[-1] %>% unlist),
#                                        .[[1]] %>% is.na ~ .x,
#                                          .default = .x))) %>% 
#       filter((.[[ncol(.)-1]] %>% as.numeric) > 35))
# names(branchen.2) <- paste0("red_",a)
# 
# c(branchen.1,
#   branchen.2) %>% 
#   write.xlsx(file = "../bericht/Tabellen/tbl_bi_branche.xlsx")
# 
# ### tbl.rati mit 1 Nachkommastelle
# # neue Funktion tbl.band.num.fnc rundet auf 1 Nachkommastelle
# tbl.rati <- map(rati.vars, ~ tbl.band.num.fnc(pp_tb, brk = breaks_tab, var = .x, weighted = T, weight = "gewicht"))
# tbl.rati.1 <- map(tbl.rati, ~ .x %>% 
#                       mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#                       rowwise %>%
#                       mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
#                              newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                              newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                              .before = 3) %>% 
#                       mutate(across(is.numeric, ~ .x %>% round(1))) %>% 
#                       ungroup)
# names(tbl.rati.1) <- map(rati.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# tbl.rati.1 %>% 
#   write.xlsx(file = "../bericht/Tabellen/tbl_rati_1nachkomma.xlsx")
# 
# 
# ### Ratingvariablen als Kategorische
# tbl.rati.2 <- list()
# for (i in 1:length(lbs.rati.1$pp.variable)) {
#   tbl.rati.2[[i]] <- tbl.band.fnc(pp_tb, var = lbs.rati.1$pp.variable[i] %>% unlist, brk = breaks_tab, weighted = T, weight = "gewicht")
# }
# tbl.rati.2 <- map(tbl.rati.2, ~ .x[[1]])
# tbl.rati.2 <- map(tbl.rati.2, ~ .x %>% 
#                       mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#                       rowwise %>%
#                       mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
#                              newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                              newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                              .before = 3) %>% 
#                       mutate(across(is.numeric, ~ .x %>% round(0))) %>% 
#                       ungroup)
# names(tbl.rati.2) <- lbs.rati.1$pp.variable
# saveRDS(tbl.rati.2, "tbl_rating_als_kategorisch.RData")
# write.xlsx(tbl.rati.2, "../bericht/Tabellen/tbl_rating_als_kategorisch.xlsx")
# 
# 
# 
# ### CLuster nach BMS/BHS und ausgewählte Breaks
# bhs <- tbl.band.fnc(pp_tb %>% filter(q01_rec == "BHS"), var = "cluster_qualitaet_3", brk = c("total","geschlecht","sprache","sozialer_hg","q27"), weighted = T, weight = "gewicht") %>% 
#   flatten %>% 
#   as_tibble %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#   select(-Gesamt)
# bms <- tbl.band.fnc(pp_tb %>% filter(q01_rec == "BMS"), var = "cluster_qualitaet_3", brk = c("total","geschlecht","sprache","sozialer_hg","q27"), weighted = T, weight = "gewicht") %>% 
#   flatten %>% 
#   as_tibble %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#   select(-Gesamt)
# q01_rec <- left_join(bms, bhs %>% select(-Fragestellung), by = "Kategorie", suffix = c("bms","bhs")) 
# 
# arbbed <- tbl.band.fnc(pp_tb, var = "cluster_qualitaet_3", brk = c("total","q16","g22_1","g22_2","g22_3","g22_4","g22_5","g22_6","g22_7","q26","q36","q43","q31_ueberstunden"), weighted = T, weight = "gewicht") %>% 
#   flatten %>% 
#   as_tibble %>% 
#   filter(Kategorie != "Nicht Gewählt") %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T)),
#          Kategorie = if_else(Kategorie == "Ja", Fragestellung %>% str_extract("(?<=\\[).+(?=\\])"), Kategorie)) %>% 
#   select(-Gesamt)
# 
# laufbahn <- tbl.band.fnc(pp_tb, var = "cluster_qualitaet_3", brk = c("total","q55","q57","q53_2"), weighted = T, weight = "gewicht") %>% 
#   flatten %>% 
#   as_tibble %>% 
#   filter(Kategorie != "Nicht Gewählt") %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T)),
#          Kategorie = if_else(Kategorie == "Ja", Fragestellung %>% str_extract("(?<=\\[).+(?=\\])"), Kategorie)) %>% 
#   select(-Gesamt)
# 
# list("q01_rec" = q01_rec, 
#            "arbbed" = arbbed, 
#            "laufbahn" = laufbahn) %>% 
#   write.xlsx("../bericht/Tabellen/tbl_cluster_breaks.xlsx")
# 
# pp %>% 
#   group_by(cluster_qualitaet_3) %>% 
#   reframe(across(c("q45_1_n","q45_5_n","q45_7_n","q45_6_n"), ~ weighted.mean(.x, gewicht, na.rm = T) %>% round(1)))
#   reframe(weighted.mean(q45_1_n, gewicht, na.rm = T))
# 
# 
# 
# ### Cluster-Darstellung
# pp %>% 
#   select(cluster_qualitaet_3,matches("q45.+_n")) %>% 
#   pivot_longer(-cluster_qualitaet_3) %>%
#   group_by(cluster_qualitaet_3,name) %>% 
#   reframe(mean = mean(value,na.rm = T)) %>% 
#   na.omit %>% 
#   ggplot(aes(x=name, y=mean, group = cluster_qualitaet_3)) +
#   geom_line(aes(color = cluster_qualitaet_3)) +
#     # theme_bw() +
#     theme(text = element_text(size = 14),
#           legend.position="bottom",
#           axis.title.x = element_blank(),
#           axis.title.y = element_blank(),
#           axis.line = element_line(size = .3),
#           panel.background = element_blank())
# 
# 
# ### Übernahmeangebot annnehmen: mehr filtern
# x <- tbl.band.fnc(pp_tb %>% filter(as.integer(q57) == 1 & as.integer(q55) == 1), var = "q56", brk = c("total","schultyp_rec"), weighted = T, weight = "gewicht") %>% 
#   flatten %>% 
#   as_tibble %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#   rowwise %>%
#   mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"),
#                     newcat = str_replace(newcat, "\\) \\(", "\\, "),
#                     newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("), 
#                     .before = 3) %>% 
#   mutate(across(is.numeric, ~ .x %>% round(1))) %>% 
#   ungroup %>% 
#   # filter(Fragestellung %>% str_detect("Gesamt|Schulform")) %>% 
#   add_row() %>% 
#   slice(which(.$Fragestellung == "Schulform"),
#                       which(.$Fragestellung %>% is.na),
#                       which(.$Fragestellung == "Gesamt")) %>% 
#   select(-c(1), -matches("Gesamt")) %>% 
#   relocate(1, .after = ncol(.)) %>% 
#   rename(q56. := 1)
# x %>% 
#   write.xlsx(file = "../bericht/Tabellen/Übernahmeangebot.xlsx")
# 
# ### Sonstige Berechnungen
# pp_tb %>% 
#   filter(q39 == "Ja, ich musste mich selber darum kümmern." | q39 == "Ja, wurde mir kostenpflichtig vom Arbeitgeber zur Verfügung gestellt.") %>% 
#   group_by(schultyp_rec) %>% 
#   # frq(q40_unterkunftskosten)
#   reframe(median = median(q40_unterkunftskosten, na.rm = T),
#           n = n()) %>% 
#   write.xlsx("../bericht/Tabellen/unterkunftkosten.xlsx")
# pp_tb %>% 
#   filter(q39 == "Ja, ich musste mich selber darum kümmern." | q39 == "Ja, wurde mir kostenpflichtig vom Arbeitgeber zur Verfügung gestellt.") %>% 
#   reframe(median = median(q40_unterkunftskosten, na.rm = T),
#           n = n()) #>% 
# 
# pp %>% 
#   # group_by(schultyp_rec) %>% 
#   reframe(mittelwert = mean(q51a_SQ001_n, na.rm = T)) %>% 
#   select(mittelwert)
# 
# 
# pp_tb %>% 
#   select(q51b) %>% 
#   distinct %>% 
#   write.xlsx("../bericht/Tabellen/praktikawegweiser_verbesserungen.xlsx")
# 
# pp_tb %>% 
#   mutate(q01_rec = if_else(q01_rec == "BHS", 1, 0)) %>% frq(q01_rec)
# 
# 
# tbl.band.fnc(pp_tb %>% filter(q48_6 == "Ja"), var = "q48_6", brk = c("total","schultyp_rec"), weighted = T, weight = "gewicht") %>% 
#   mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>% 
#   rowwise %>%
#   mutate(newcat = paste0(Kategorie, " (n=", c_across(last_col()),")"), # Kategorie mit stichprobengr??e
#          newcat = str_replace(newcat, "\\) \\(", "\\, "),
#          newcat = str_replace(newcat, "\\- \\(", "Gesamt \\("),
#          .before = 3) %>%
#          mutate(across(is.numeric, ~ .x %>% round(0)),
#          Kategorie = str_remove(Kategorie, "\\)")) %>%
#   ungroup %>%
#   tidyr::fill(Fragestellung, .direction = "down") %>% # Nur Zeilen mit Schulform und Gesamt ?bernehmen und umbauen f?r effizientere Verwendung bei Erstellung der Grafiken
#   filter(Fragestellung %>% str_detect("Gesamt|Schulform")) %>%
#   add_row() %>%
#   slice(which(.$Fragestellung == "Schulform"),
#   which(.$Fragestellung %>% is.na),
#   which(.$Fragestellung == "Gesamt")) %>%
#   select(-c(1), -matches("Gesamt")) %>%
#   relocate(1, .after = ncol(.)) %>% 
#   mutate(a = as.numeric(`n (gültig)`)/541*100,
#          across(a, ~ format(round(.x, 1), big.mark = ".", decimal.mark = ",", trim = T)))
#   
# 
# 
# ### Einkommen - Kosten
# a <- pp_tb %>%
#   filter(q38_eink_gesamt > 100) %>%
#   reframe(q38_einkommen_gesamt_kat = "Gesamt",
#           n = n(),
#           mittl_kosten = weighted.mean(q40_kosten_gesamt, gewicht, na.rm = T),
#           median_kosten = median(q40_kosten_gesamt, na.rm = T),
#           mittl_einkommen = weighted.mean(q38_eink_gesamt, gewicht, na.rm = T),
#           median_einkommen = median(q38_eink_gesamt, na.rm = T),
#           kosten_zu_eink = weighted.mean(q40_kosten_gesamt/q38_eink_gesamt, na.rm = T))
# 
# pp_tb %>%
#   # filter(q38_eink_gesamt > 100) %>%
#   group_by(q38_einkommen_gesamt_kat) %>%
#   reframe(n = n(),
#           # max_kosten = max(q40_kosten_gesamt, na.rm = T),
#           # min_kosten = min(q40_kosten_gesamt, na.rm = T))
#           mittl_kosten = weighted.mean(q40_kosten_gesamt, gewicht, na.rm = T),
#           median_kosten = median(q40_kosten_gesamt, na.rm = T),
#           mittl_einkommen = weighted.mean(q38_eink_gesamt, gewicht, na.rm = T),
#           median_einkommen = median(q38_eink_gesamt, na.rm = T),
#           kosten_zu_eink = weighted.mean(q40_kosten_gesamt/q38_eink_gesamt, gewicht, na.rm = T)) %>% 
#   mutate(n = paste0(q38_einkommen_gesamt_kat, " (n=",n,")")) %>% 
#   rbind(a) %>% 
#   mutate(kosten_zu_eink = round(kosten_zu_eink*100,1)) %>% 
#   na.omit %>% 
#   write.xlsx("../bericht/Tabellen/einkommen_ausgaben.xlsx")
# 
# 
# ### Stellenwert - Ausland
# pp_tb %>% 
#   group_by(regional) %>% 
#   reframe(angestrengt = mean(g20_2_n, na.rm = T),
#           gut = mean(g20_1_n, na.rm = T))
# 
# 
# lbl.var.df %>% 
#   filter(pp.variable %>% str_detect("schultyp_rec"))
# 
# map(c("q19_1","q19_2","q19_3"), ~ pp_tb %>% 
#       group_by(q24) %>% 
#       frq(.x))
#   

tbl.list <- readRDS("tbl_list.RData")
tbl.list$tbl.kat$q56 %>% 
  mutate(across(where(is.numeric), ~ format(.x %>% round(0), big.mark = ".", decimal.mark = ",", trim = T)))
mutate(Kat = Kategorie,
       Kategorie = Kategorie %>% 
         str_remove("\\)") %>% 
         paste0(", n=",`n (gültig)`,")"),
       Kategorie = if_else(Kategorie %>% str_detect("-, "), 
                           paste0(Fragestellung, "(n=",`n (gültig)`,")"),
                           Kategorie)) %>% 
  # mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>%
  select(-Gesamt) %>% 
  write.xlsx("../bericht/Tabellen/q56.xlsx")


kat <- c("q37",
         "q31_ueberstunden",
         "q32",
         "q55",
         "q56",
         "cluster_qualitaet_3",
         "q40_kosten_gesamt_kat")

tbl.list$tbl.kat$Bundesland %>% 
  mutate(id = 1:nrow(.), .before=1) %>% 
  View

tbl.list$tbl.kat[names(tbl.list$tbl.kat) %in% kat] %>%
  map(~ .x %>% 
        mutate(across(where(is.numeric), ~ format(.x %>% round(0), big.mark = ".", decimal.mark = ",", trim = T)),
               across(-c(1:2,ncol(.)), ~ .x %>% paste0("%"))) %>%
        mutate(Kat = Kategorie,
               Kategorie = Kategorie %>% 
                 str_remove("\\)") %>% 
                 paste0(", n=",`n (gültig)`,")"),
               Kategorie = if_else(Kategorie %>% str_detect("-, "), 
                                   paste0(Fragestellung, "(n=",`n (gültig)`,")"),
                                   Kategorie)) %>% 
        # mutate(across(ncol(.), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))) %>%
        select(-Gesamt) %>% 
        fill(Fragestellung) %>% 
        slice(67:80,1)) %>% 
  write.xlsx("../bericht/Tabellen/branche_bivariat.xlsx")  
system("open ../bericht/Tabellen/branche_bivariat.xlsx")


x <- tbl.band.fnc(pp_tb, var = "q53_2", brk = breaks_tab, weighted = T, weight = "gewicht")

x[[1]] %>% 
  slice(nrow(.))

system("open ../bericht/Tabellen/q56.xlsx")


### Vergleich 2022 - 2024 ----
### Für den Vergleich vorbereiten: Pädagogik und Soziales herausnehmen
pp3vg <- pp_tb %>% 
  filter(schultyp_rec %>% as_numeric < 9)

a <- c("q16", # das sind die Variablen für den Vergleich
       "q17_",
       "g21_",
       "q31_ueberstunden",
       "q32",
       "q37",
       "q38_einkommen_gesamt_kat",
       "q23_",
       "q45_") %>% 
  paste0(collapse = "|")
pp3vg %>% # damit gebe ich mir die Liste der Namen aus
  select(matches(a)) %>% 
  names %>% 
  str_subset(".+n$",negate = T) %>% 
  cat(sep = '", "')
b <- list("q16", # zu einer gemeinsamen Liste mit gruppierten Mehrfachantworten 
          c("q17_1", "q17_2", "q17_3", "q17_4", "q17_5", "q17_6", "q17_7", "q17_8", "q17_9", "q17_10", "q17_11"),
          "q18_anz_bew_kat2",
          c("g21_1", "g21_2", "g21_3", "g21_4", "g21_5", "g21_6", "g21_7", "g21_8", "g21_9"), 
          c("q23_1", "q23_2", "q23_3", "q23_4", "q23_5", "q23_6", "q23_7", "q23_8", "q23_9", "q23_10", "q23_11", "q23_12", "q23_13"),
          "q31_ueberstunden","q32", "q37", 
          "q38_einkommen_gesamt_kat", "q23_unterstuetzung")

vgl <- list() 
for (i in 1:length(b)) { # tbl-Auswertung
  vgl[[i]] <- tbl.band.fnc(pp3vg, var = b[[i]], brk = c("total"), weighted = T, weight = "gewicht")
}
names(vgl) <- c("q16","q17","q18","g21","q23", "q31_ueberstunden", "q32", "q37", "q38_ek_ges_kat", "q23_unterstuetzung")

y <- reduce(map(vgl[["q23"]], ~ .x %>% select(Ja)), function(x,y) cbind(x,y)) # q23 soll als Mehrfachantwortset ausgewertet werden
names(y) <- letters[1:length(y)]
dims <- dim(vgl[["q23"]][[1]])
out <- vgl[["q23"]][[1]] %>%
  select(-c(3:(dims[2]-2))) %>% 
  cbind(y) %>% 
  as_tibble() %>% 
  relocate(c(3,4), .after=last_col())
vgl[["q23"]] <- out

vgl$q38_ek_ges_kat <- tbl.band.fnc(pp3vg %>% filter(q31_ueberstunden %>% as.integer >= 3), var = "q38_einkommen_gesamt_kat", brk = c("total"), weighted = T, weight = "gewicht")

vgl <- map2(vgl,c("q16","q17","q18","g21","q23","q31_ueberstunden", "q32", "q37", "q38_einkommen_gesamt_kat", "q23_unterstuetzung"), ~.x %>% 
              as.data.frame() %>% 
              mutate(var = .y, .before = 1))

vgl.r <- tbl.band.num.fnc(pp3vg, var = c("q45_1", "q45_2", "q45_3", "q45_4", "q45_5", "q45_6", "q45_7", "q45_8", "q45_9", "q45_10"), brk = c("total"), weighted = T, weight = "gewicht") 

a <- map(vgl, ~ .x %>% as.data.frame())
a[["q45"]] <- vgl.r %>% as.data.frame()

### Einkommen Vergleich mit > 35h Arbeitszeit
ek <- list(pp3vg %>% 
             filter(q31_ueberstunden %>% as.integer >= 3) %>% 
             count(q38_einkommen_gesamt_kat, wt = gewicht, name = "einkommen_2024") %>% 
             na.omit %>% 
             mutate(perc = einkommen_2024/sum(einkommen_2024)*100) %>% 
             adorn_totals() %>% 
             mutate(across(where(is.numeric), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T))),
           pp2 %>% 
             filter(q31_ueberstunden %>% as.integer >= 3) %>% 
             count(q37_einkommen_gesamt_kat, wt = gewicht, name = "einkommen_2022") %>% 
             na.omit %>% 
             mutate(perc = einkommen_2022/sum(einkommen_2022)*100) %>% 
             adorn_totals() %>% 
             mutate(across(where(is.numeric), ~ format(.x, big.mark = ".", decimal.mark = ",", trim = T)))
) 

a[["q38_eink_ges_2024"]] <- ek[[1]]
a[["q37_eink_ges_2022"]] <- ek[[2]]
a %>% 
  write.xlsx("../bericht/Tabellen/vgl_gesamt.xlsx")


### Cluster
pp2 %>% 
  select(matches("q42_\\d+$")) %>% 
  mutate_all(as.numeric) %>% 
  reframe(across(everything(), ~ mean(.x, na.rm = T)))


### Unterschiede bei Einkommensverteilungen
lbl.var.df %>%
  filter()
x <- pp_tb %>% 
  # frq(schultyp_rec %>% as_numeric)
  filter(q38_einkommen_gesamt_kat %>% as_numeric() >=9 &
           schultyp_rec %>% as_numeric < 9) %>% 
  group_by(schultyp_rec) %>% 
  count(wt=gewicht) %>% 
  mutate(p = n/5965*100) 

breaks_tab_2 <- pp2_tb %>% select(total, q1_rec, richtung_geschlecht, q1_schultyp, q4, q28_rec, q57a_rec, q59_sozialer_hintergrund, q58_notendurchschnitt, q24_rec, q27_rec, q36, q31_ueberstunden, q51, q53_rec) %>% names()
ek.vgl <- tbl.band.fnc(pp2_tb %>% filter(q31_ueberstunden %>% as.integer >= 3), var = "q37_einkommen_gesamt_kat", brk = breaks_tab_2, weighted = T, weight = "gewicht")[[1]] 

ek.vgl %>% 
  write.xlsx("../bericht/Tabellen/pp2_einkommen_tb.xlsx")


pp2 %>% 
  # frq(q1)
  # get_label() %>% 
  # named.to.df.fnc() %>% 
  # filter()
  # frq(schultyp_rec %>% as_numeric)
  filter(q37_einkommen_gesamt_kat %>% as_numeric() >=9) %>% 
  group_by(q1) %>% 
  count(wt=gewicht) %>% 
  mutate(p = n/5534*100) %>% 
  cbind(x %>% select(p))


## falsche gewichtung ----

# Durch eine Unachtsamkeit hatte ich statt der Gewichte die Rückläufe verwendet. Anbei eine Syntax, um die Auswirkung dieser Verwechslung sichtbar zu machen.

alt <- readRDS("falsche-gewichtung/tbl_2gether.RData")
names(alt) == names(tbl.list.2gether)
x <- map2(alt, tbl.list.2gether, ~ select(.x, where(is.numeric)) - select(.y, where(is.numeric))) # hiermit sehe ich mir die Differnzen zwischen den beiden Ergebnissen an
for (i in 1:length(x)) {
  x[[i]] <- alt[[i]][1:2] %>% cbind(x[[i]]) %>% 
    as_tibble # man könnte hier noch nach jenen Zeilen filtern, in denen zumindest 1 Differenz größer als z.B. 4 ist.
}


## Wünsche von Boris ----

# Siehe Ordner Kommunikation




# Individualiserte Auswertung ----

## Schulen ----


library(rstudioapi)

jobRunScript(
  # "auswertung-schulen.R",
  "auswertung-BL.R",
  # "background.R",
  name = NULL,
  encoding = "latin1",
  workingDir = NULL,
  importEnv = FALSE,
  exportEnv = ""
)



# weitere Auswertungen ----

correl <- cor(pp %>% 
     select(matches("q23.+_n")),
    use = "complete.obs"
)
corrplot::corrplot(correl, method = "circle", order = "AOE")

fa.parallel(
  pp %>% 
    select(matches("q23.+_n")),
  fa = "fa"
)
fa(pp %>% 
     select(matches("q23.+_n")),
   nfactors = 3)


# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++##
# Latente Klassenanalyse (LCA) ? Ausbildungsverbund-Betriebe
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
library(poLCA)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 1. Daten vorbereiten
#    poLCA erwartet ganzzahlige Werte >= 1 (hier: 1 = "Trifft nicht zu", 2 = "Trifft zu")
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df_lca <-
  pp %>% 
  select(matches("q23.+_n")) %>%
  mutate(across(-0, ~ .x + 1)) %>% 
  filter(!if_any(-0, is.na))

# Modellformel
df_lca %>% names() %>% cat(sep = ", ")
f <- cbind(q23_1_n, q23_2_n, q23_3_n, q23_4_n, q23_5_n, q23_6_n, q23_7_n, q23_8_n, q23_9_n, q23_10_n, q23_11_n, q23_12_n, q23_13_n) ~ 1

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 2. Modelle mit K = 2?5 Klassen fitten und vergleichen
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

set.seed(4821)
fits <- map(2:5, \(k) poLCA(f, df_lca, nclass = k, maxiter = 2000, nrep = 100, verbose = FALSE))

tibble(
  K      = 2:5,
  AIC    = map_dbl(fits, "aic"),
  BIC    = map_dbl(fits, "bic"),
  logLik = map_dbl(fits, "llik")
)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 3. Beste L?sung: K = 3 (Kompromiss zwischen AIC und BIC)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

best3 <- fits[[2]]

# Klassengr??en (Anteile)
round(best3$P, 3)

prob_long <-
  imap(
    best3$probs,
    ~ as.data.frame(.x) %>%
      rownames_to_column("Klasse") %>%
      pivot_longer(
        starts_with("Pr"),
        names_to = "Antwort",
        values_to = "prob"
      ) %>%
      mutate(Item = .y)
  ) %>%
  list_rbind()

#### nur hohe Wahrscheinlichkeiten ----
prob_long %>%
  filter(prob > 0.7)

#### Modalprofil bestimmen -----
prob_long %>%
  group_by(Item, Klasse) %>%
  slice_max(prob, n = 1) %>%
  ungroup() %>%
  print(n=Inf)

# Itemwahrscheinlichkeiten P("Trifft zu") pro Klasse
probs3 <- map_dfr(best3$probs, \(m) m[, 2] %>% round(3), .id = "item")
probs3

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 4. Modellg?te: Entropie und mittlere Klassifikationswahrscheinlichkeiten (AvePP)
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

entropy <- function(p) { # Ramaswamy et al. (1993),; h?ufig bei Vermunt & Magidson zitiert
  K <- ncol(p)
  N <- nrow(p)
  1 - (-sum(p * log(p + 1e-10))) / (N * log(K))
}

cat("Entropie:", round(fnc.entropy.r2(best3), 3), "\n")

pred_class <- best3$predclass
post       <- best3$posterior

ave_pp <- tibble( # Die Idee stammt aus Nagin (2005) und wird z.B. bei Clark & Muth?n (2009) als G?tekriterium f?r LCA/GMM empfohlen (Daumenregel: AvePP = 0.70 pro Klasse gilt als akzeptabel)
  Klasse = 1:3,
  Anteil = round(best3$P, 3),
  AvePP  = map_dbl(1:3, \(k) mean(post[pred_class == k, k]))
) |>
  mutate(across(c(Anteil, AvePP), \(x) round(x, 3)))

ave_pp

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 6. Profildiagramm
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

item_labels <- c(
  q46_1 = "Lernfortschritt\nkontinuierlich dokumentiert",
  q46_2 = "Lernergebnisse\nkompetenzorientiert beschrieben",
  q46_3 = "Interne Vorbereitung\nvor Beginn des AV",
  q46_4 = "Konkrete Ansprechperson\nf?r Lehrling im AV",
  q46_5 = "Strukturierter Austausch\nzwischen Betrieben/Anbietern",
  q46_6 = "Strukturierter Austausch\nmit dem Lehrling",
  q46_7 = "Berufsschule wird ?ber\nTeilnahme am AV informiert",
  q46_8 = "Digitale Lernplattform\nim Einsatz",
  q46_9 = "Qualit?tssicherung\nim AV"
)

klassen_labels <- c(
  "class 1: " = "Klasse 1: Vollst?ndig formalisiert (18%)",
  "class 2: " = "Klasse 2: Strukturiert, wenig formalisiert (43%)",
  "class 3: " = "Klasse 3: Gering formalisiert (39%)"
)

probs3 |>
  pivot_longer(-item, names_to = "Klasse", values_to = "prob") |>
  mutate(
    item_label = factor(item_labels[item], levels = item_labels),
    Klasse     = klassen_labels[Klasse]
  ) |>
  ggplot(aes(x = item_label, y = prob, color = Klasse, group = Klasse)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1),
                     breaks = seq(0, 1, 0.25)) +
  scale_color_manual(values = c("#1b6ca8", "#e07b39", "#3aab6d")) +
  labs(
    title = "Profildiagramm: Klassen der Ausbildungsverbund-Betriebe",
    x = NULL, y = "Anteil 'Trifft zu'", color = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text.x    = element_text(size = 8, angle = 35, hjust = 1),
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 5. Klassenzugeh?rigkeit in df speichern
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

df <-
  df %>%
  select(-matches("lca_klasse")) %>%
  left_join(data.frame(id.x = df_lca$id.x,
                       lca_klasse = best3$predclass)) %>%
  mutate(lca_klasse = factor(lca_klasse,
                             levels = 1:3,
                             labels = c(
                               "Vollst?ndig formalisiert",
                               "Strukturiert, wenig formalisiert",
                               "Gering formalisiert"
                             )
  ))



# Alt ----
## Tabellenband generieren

# Optionsantwort
tab_tb_q27 <- tabellenband_bivariat(pp_tb, row_vars = all_of(breaks_tab), col_vars = q27, version = "summary", weight = gewicht, weighted = T, show_n = T, pub_reduced = T) %>%
  set_names("q27")

# Mehrfachantwort
tab_tb_q44 <- table_mc_bi(pp_tb, row_vars = all_of(breaks_tab), col_vars = c(q44_1:q44_5, q44_other2), weight = gewicht, weighted = T, show_n = T, pub_reduced = T) %>% list() %>% set_names("q44")

# Mittelwert
tab_tb_q49 <- table_mean_bi(pp_tb, all_of(breaks_tab), col_vars = c(q49_1_n:q49_4_n), version = "summary", weighted = T, weight = gewicht, pub_reduced = T) %>% list() %>% set_names("q49_arbeitsmarkt")


obj <- Filter(function(x) inherits(get(x), c("list")), ls())
obj <- obj[str_detect(obj, "tab_tb")]

list_export <- c()

for (i in 1:length(obj)) {
  list_export <- c(list_export, get(obj[i]))
  print(obj[i])
}

write.xlsx(list_export, "pp_tabellenband_berichtanhang.xlsx", overwrite = T, startRow = 1)





## Tabellenband

## Bivariat - PP


# bivariat.varnames <- pp.alt %>% 
#   select(q1, q1_schulform, q4, q12, q13_1:q13_7, q13_other2, q14_anz_bew_kat2, q17_1:q17_3, q18_1:q18_3, q19_1:q19_9, 
#          q19_other2, q20, q22, q22a_1:q22a_13, q23:q30,q31_ueberstunden, q32, q33_1:q33_2, q34, q35_1:q35_9, q35_other2, q36,
#          q37a, q37_einkommen_kat, q37_taschengeld_kat, q37_trinkgeld_kat, q37_einkommen_gesamt_kat,
#          q38, q39_kosten_gesamt_kat, q40, q41_1:q41_5, q42_1:q42_10, q43_1:q43_11, q44_1:q44_5, q44_other2,
#          q45_1:q45_7, q45_other2, q46, q47, q48_1:q48_6, q48_other2, q49_1:q49_4, q50_1:q50_7,
#          q51:q55, q57a, q58_1:q58_3, q58_notendurchschnitt) %>% 
#   names
# 
# bivariat.varnames.1 <- lbl.var.df %>%
#   filter(pp.alt.variable %in% bivariat.varnames) %>%
#   filter(!is.na(pp.variable)) %>%
#   select(pp.variable) %>%
#   unlist %>%
#   as.vector %>%
#   unique

breaks <- readRDS("breaks.RData")

bivariat.varnames <- lbl.var.df %>% 
  filter(var.typ == "Kategorisch") %>% 
  select(pp.variable) %>% 
  unique %>% 
  na.omit %>% 
  unlist %>% 
  as.vector

### Manche Filterfragen enthalten keine Antworten, was bei der Auswertung zu Problemen führt. Diese sollten im Vorhinein herausgefiltert werden.

excl <- pp %>%
  select(matches("_total$")) %>% 
  select(where(~ sum(.x, na.rm = T) == 0)) %>% 
  names %>% 
  str_remove("_.+") %>% 
  paste0(collapse = "|")
bivariat.varnames.1 <- bivariat.varnames[bivariat.varnames %>% str_detect(excl, negate = T)]

excl.1 <- pp %>%
  select(matches("_other$")) %>% 
  names
bivariat.varnames.1 <- bivariat.varnames.1[bivariat.varnames.1 %in% excl.1 == F]



### Tabellen
tab_bi <- map(bivariat.varnames.1, ~ tabellenband_bivariat(pp, all_of(breaks), 
                                                           col_vars = .x %>% as.name,
                                                           weight = gewicht, weighted = T))
saveRDS(tab_bi,"tab_bi.RData")
# tab_bi <- readRDS("tab_bi.RData")

tab_bi_1 <- map(tab_bi, ~ .x[[1]]) ### Martins Funktion erzeugt eine zusätzliche Listenebene. Dadurch fehlen beim Auswurf als Excel Arbeitsmappen- und Variablenbezeichnungen.
tab_bi_1 <- set_names(tab_bi_1,bivariat.varnames.1)

saveRDS(tab_bi_1,"tab_bi_1.RData")
write.xlsx(tab_bi_1, "pp_bivariat_tabelle.xlsx", overwrite = T, startRow = 1)



## Bivariat - PP - Cluster


breaks <- readRDS("breaks.RData")

varnames <- c("cluster_qualitaet_2","cluster_qualitaet_4")

# tab_bi_cluster <- map(varnames, ~ tabellenband_bivariat(pp, all_of(breaks), col_vars = .x %>% as.name,
#                                           weight = gewicht, weighted = T))
x <- tabellenband_bivariat(pp, all_of(breaks), col_vars = "cluster_qualitaet_2",
                           weight = gewicht, weighted = T)

.x <- breaks[1]
# %>% paste0(collapse = "|")
# tab_bi_cluster <- map_dfr(breaks, ~
pp %>%
  group_by(!!sym(.x),cluster_qualitaet_2) %>% 
  reframe(n = n()) %>% 
  group_by(!!sym(.x)) #%>%
mutate(p = round(n/sum(n)*100,1)) %>% 
  bind_rows(reframe(., 
                    !!sym(.x) := !!sym(.x),
                    cluster_qualitaet_2 = "total",
                    n = sum(n),
                    p = round(sum(p),0)) %>%
              unique)# %>% 
ungroup %>% 
  arrange(!!sym(.x)) %>% 
  pivot_longer(-c(.x,cluster_qualitaet_2)) %>% 
  pivot_wider(names_from = cluster_qualitaet_2, values_from = c(value)) %>% 
  arrange(name) %>% 
  mutate(name = ifelse(name == "n", "n (ungewichtet)", "% gültig (gewichtet)"),
         !!sym(.x) := as.character(!!sym(.x)),
         !!sym(.x) := ifelse(!!sym(.x) %>% is.na, "(Fehlend)",!!sym(.x))) %>% 
  mutate(Item = "cluster_qualitaet_2", 
         Variable = .x,
         .before = 1) %>% 
  mutate(across(c(Item,Variable), ~ unfill_vec(.))) %>% 
  rename("Kategorie" = .x,
         # Variable = !!sym(.x),
         stat = name,
         Gesamt = total,
         "(Fehlend)" = "NA") %>% 
  relocate(stat, .after = Kategorie) %>% 
  relocate("(Fehlend)", .after = Gesamt) %>% 
  mutate(typ = "einzel")
# )

pp %>% 
  # select(!!sym(.x))
  group_by(cluster_qualitaet_2) %>%
  reframe(n = n()) %>% 
  # group_by(!!sym(.x)) %>%
  mutate(p = round(n/sum(n)*100,1)) %>% 
  # na.omit %>% 
  pivot_longer(-cluster_qualitaet_2, names_to = "Variable") %>% 
  pivot_wider(names_from = cluster_qualitaet_2, values_from = value) %>% 
  rowwise %>% 
  mutate(Variable = NA,
         Gesamt = sum(c_across(-Variable))) %>% 
  ungroup %>% 
  mutate(stat= c("n (ungewichtet)",
                 "% gültig (gewichtet)")) %>% 
  mutate(Item = "cluster_qualitaet_2", 
         Variable = .x,
         .before = 1) %>% 
  mutate(across(c(Item,Variable), ~ unfill_vec(.))) %>% 
  mutate(Item = NA,
         Variable = NA,
         Kategorie)

rename("Kategorie" = .x,
       # Variable = !!sym(.x),
       stat = name,
       Gesamt = total,
       "(Fehlend)" = "NA") %>% 
  relocate(stat, .after = Kategorie) %>% 
  relocate("(Fehlend)", .after = Gesamt) %>% 
  mutate(typ = "einzel")


reframe(!!sym(.x) := !!sym(.x),
        n = sum(n),
        p = 100) #%>%
# bind_rows(reframe(.,
#                   !!sym(.x) := !!sym(.x),
#                   n = sum(n),
#                   p = 100)) #%>%


# tab_bi_cluster_1 <- map(tab_bi_cluster, ~ .x[[1]])

saveRDS(tab_bi_cluster,"tab_bi_cluster.RData")
write.xlsx(tab_bi_cluster, "pp_bivariat_cluster_tabelle.xlsx", overwrite = T, startRow = 1)


## Bivariat - PP - Tabellenband

## alle Tabellen zusammenfassen
write.xlsx(c(tab_uni, tab_uni_dispens, tab_bi_1, tab_bi_cluster), "pp_tabellenband.xlsx", overwrite = T, startRow = 1)



## Bivariat - PP - Summary

### Letzter Stand
#andere Breaks wegen Kreuzungen definieren.

breaks2 <- pp %>% select(q1, q1_schulform, q1_schultyp, q4, q36, q40, q51, q53, q55, q57a, q58_notendurchschnitt, q59_sozialer_hintergrund) %>% names()

# q12
tab1_q1_12 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q12), version = "summary", weighted = T, weight = gewicht)

# q13
tab1_q13 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q13_1:q13_7, q13_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q13_schw_stelle")

#q14
tab1_q14 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q14_anz_bew_kat2), version = "summary", weighted = T, weight = gewicht)

# q17
tab1_q17 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q17_1:q17_3), weighted = T, weight = gewicht) %>% list() %>% set_names("q17_ort_suche")

#q18
tab1_q18 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q18_1_n:q18_3_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q18_einstellung_prakt")

# q19
tab1_q19 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q19_1:q19_9, q19_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q19_motive_praktikum")

# q20-q22a
tab1_q20_22 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q20, q21, q22), version = "summary", weighted = T, weight = gewicht)

# q22a
tab1_q22a <- table_mc_bi(pp, all_of(breaks), col_vars = c(q22a_1:q22a_13), weighted = T, weight = gewicht) %>% list() %>% set_names("q22_vorbereitung_schule")

# q23_34
tab1_q23_34 <- tabellenband_bivariat(pp, all_of(breaks2), col_vars = c(q23:q30, q31_ueberstunden, q33_1:q33_2, q34), version = "summary", weighted = T, weight = gewicht)

# q35
tab1_q35 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q35_1:q35_9, q35_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q35_informiert_vonwem")

# q36-39
tab1_q36_q39 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q36")]), col_vars = c(q36, q37a, q38, q39a, q39b), version = "summary", weighted = T, weight = gewicht) 

# q37
tab1_q36_q39 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q37_einkommen_kat, q37_trinkgeld_kat, q37_taschengeld_kat, q37_einkommen_gesamt_kat, q37_einkommen_systematik), version = "summary", weighted = T, weight = gewicht)

tab1_q39 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q39_essenskosten_kat, q39_fahrtkosten_kat, q39_arbeitskosten_kat), version = "summary", weighted = T, weight = gewicht)

# q40
tab1_q40 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q40")]), col_vars = c(q39b, q40), version = "summary", weighted = T, weight = gewicht)

# q41
tab1_q41 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q41_1_n:q41_5_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q41_betreuung_betrieb")

# q42
tab1_q42 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q42_1_n:q42_9_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q42_arbeitsbed")

# q43
tab1_q43 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q43_1_n:q43_11_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q43_lernbed")

# q44
tab1_q44 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q44_1:q44_5, q44_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q44_nachweise")

# q45
tab1_q45 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q45_1:q45_7, q45_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q45_dokumentation")

# q46_47
tab1_q46_47 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q46, q47), version = "summary", weighted = T, weight = gewicht)

# q48
tab1_q48 <- table_mc_bi(pp, all_of(breaks), col_vars = c(q48_1:q48_6, q48_other2), weighted = T, weight = gewicht) %>% list() %>% set_names("q48_nachbesprechung")

# q49 
tab1_q49 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q49_1_n:q49_4_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q49_arbeitsmarkt")

# q50 
tab1_q50 <- table_mean_bi(pp, all_of(breaks), col_vars = c(q50_1_n:q50_7_n), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q50_bewertung_praktikum")

# q51
tab1_q51 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q51")]), col_vars = c(q51), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q51")

# q52
tab1_q52 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q52")]), col_vars = c(q52), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q52")

# q53
tab1_q53 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q53")]), col_vars = c(q53), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q53")

# q54
tab1_q54 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q54")]), col_vars = c(q54), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q54")

# q55
tab1_q55 <- tabellenband_bivariat(pp, all_of(breaks[!breaks %in% c("q55")]), col_vars = c(q55), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q55")

# q35
tab1_q35 <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(q35b, q35c), version = "summary", weighted = T, weight = gewicht)



obj <- Filter(function(x) inherits(get(x), c("list")), ls())
obj <- obj[str_detect(obj, "tab1_")]

list_export <- c()

for (i in 1:length(obj)) {
  list_export <- c(list_export, get(obj[i]))
  print(obj[i])
}

write.xlsx(list_export, "pp_tabellenband_summary.xlsx", overwrite = T, startRow = 1)



## Sonstige Antworten

## Offene Fragen
tab2_q8_other <- pp %>% count(q8_other) %>% list() %>% set_names("q8_other")
tab2_q9_other <- pp %>% count(q9_other) %>% list() %>% set_names("q9_other")
tab2_q13_other <- pp %>% count(q13_other) %>% list() %>% set_names("q13_other")
tab2_q19_other <- pp %>% count(q19_other) %>% list() %>% set_names("q19_other")
tab2_q20_other <- pp %>% count(q20_other) %>% list() %>% set_names("q20_other")
tab2_q35_other <- pp %>% count(q35_other) %>% list() %>% set_names("q35_other")
tab2_q35a <- pp %>% count(q35a) %>% list() %>% set_names("q35a")
tab2_q44_other <- pp %>% count(q44_other) %>% list() %>% set_names("q44_other")
tab2_q45_other <- pp %>% count(q45_other) %>% list() %>% set_names("q45_other")
tab2_q48_other <- pp %>% count(q48_other) %>% list() %>% set_names("q48_other")
tab2_q57_other <- pp %>% count(q57_other) %>% list() %>% set_names("q57_other")
tab2_q8_other <- pp %>% count(q8_other) %>% list() %>% set_names("q8_other")
tab2_q8_other <- pp %>% count(q8_other) %>% list() %>% set_names("q8_other")




obj <- Filter(function(x) inherits(get(x), c("list")), ls())
obj <- obj[str_detect(obj, "tab2_")]

list_export <- c()

for (i in 1:length(obj)) {
  list_export <- c(list_export, get(obj[i]))
  print(obj[i])
}

write.xlsx(list_export, "pp_sonstige_antworten.xlsx", overwrite = T, startRow = 1)


## Individuelle Auswertungen und Zusammenhänge

## PP Dispens

pp_dispens %>% summarise(mean(q10_anteil_antworten, na.rm = T))
pp_dispens %>% summarise(mean(q10_corona_absage, na.rm = T))

## PP

# Über Rechte Bescheid wissen
x <- lbl.var.df %>% 
  filter(pp.label %>% str_detect("Recht|recht")) %>% 
  select(pp.variable) %>% 
  unlist %>% 
  as.vector

tab_rechte <- tabellenband_univariat(pp, x)
write.xlsx(tab_rechte, "Sonderauswertung/Zwischenstand/pp_sonder_rechte.xlsx")

## Soziodemografische Struktur
tab3_q1 <- tabellenband_bivariat(pp_origin, c(q55, q57a, q59_sozialer_hintergrund), col_vars = c(q1), weighted = T, weight = gewicht, perc = "col") %>% set_names("q1_original")



tabellenband_bivariat(pp, q12, col_vars = c(q14_anz_bew_kat2), version = "summary", weighted = T, weight = gewicht)[[1]] %>% View()
cor.test(as.numeric(pp$q12), pp$q14_anz_bew)

# Berechnugn des zeitlichen Abstandes
pp$q15_diff_suche_zusage <- interval(pp$q15, pp$q15a)
pp$q15_diff_suche_zusage <- pp$q15_diff_suche_zusage %/% months(1)

pp$q15_diff_suche_antritt <- interval(pp$q15, pp$q16)
pp$q15_diff_suche_antritt <- pp$q15_diff_suche_antritt %/% months(1)

# Jahr der Durchführung des Pflichtpraktikums
pp$jahr_praktikum <- year(pp$q16)
pp <- pp %>% mutate(jahr_praktikum = if_else(jahr_praktikum > 2021 | jahr_praktikum <= 2016, NA_real_, jahr_praktikum), jahr_praktikum = as.factor(jahr_praktikum))

#pp$jahr_praktikum_monate <- month(pp$q16, label = T)
pp <- pp %>% mutate(q16_rec = if_else(year(q16) > 2021 | year(q16) <= 2016, NA_POSIXct_, q16), 
                    jahr_praktikum_monate = month(q16_rec, label = T))



tab3_jahr_praktikum <- tabellenband_bivariat(pp, all_of(breaks), col_vars = c(jahr_praktikum, jahr_praktikum_monate), version = "summary", weighted = T, weight = gewicht)


# Berechnung des Absagenanteils
tab3_absagen <- table_mean_bi(pp, c(q1, q1_schulform, q1_schultyp, q14_anz_bew_kat2), col_vars = c(q14_anteil_antworten), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q14_rueckmeldungen")

tab3_corona_absagen <- table_mean_bi(pp %>% filter(jahr_praktikum == "2020" | jahr_praktikum == "2021"), c(q1, q1_schulform, q1_schultyp, q14_anz_bew_kat2), col_vars = c(q14_corona_absagegrund), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q14_absagen_coronabedingt")

# Anstrenungen vs. Anzahl an Bewerbungen
table_mean_bi(pp, c(q14_anz_bew_kat2), col_vars = c(q18_1_n), version = "summary", weighted = T, weight = gewicht)

table_mean_bi(pp, c(q12), col_vars = c(q18_3_n), version = "summary", weighted = T, weight = gewicht)
cor.test(as.numeric(pp$q12), pp$q18_3_n)

# Stundenanzahl vs. Zeitpunkt des Praktikums
tabellenband_bivariat(pp, q30, col_vars = c(q31_ueberstunden), version = "summary", weighted = T, weight = gewicht)[[1]] %>% View()

# vertragliche Unklarheiten
tabellenband_bivariat(pp, c(q33_1, q33_2, q34, q31_ueberstunden), q36, version = "summary", weighted = T, weight = gewicht)[[1]] %>% View()

# Einkommen

q37_einkommen_median <- pp %>% filter(q37_einkommen > 0) %>% group_by(q1) %>% 
  summarise(across(c(q37_einkommen, q37_taschengeld, q37_trinkgeld, q37_eink_gesamt), ~median(.x, na.rm = T)))

pp %>% filter(q37_einkommen > 0) %>% 
  summarise(across(c(q37_einkommen, q37_taschengeld, q37_trinkgeld, q37_eink_gesamt), ~median(.x, na.rm = T)))

q37_einkommen_median_n <- pp %>% filter(q37_einkommen > 0) %>% group_by(q1) %>% 
  summarise(across(c(q37_einkommen, q37_taschengeld, q37_trinkgeld, q37_eink_gesamt), ~sum(!is.na(.x)), .names = "{.col}_n"))

tab3_q37_einkommen_median <- q37_einkommen_median %>% left_join(q37_einkommen_median_n) %>% list() %>% set_names("q37_einkommen_median")

tab3_einkommen_sytematik <- tabellenband_bivariat(pp, c(q1), q37_einkommen_systematik2, version = "summary", weighted = T, weight = gewicht)

tabellenband_bivariat(pp, c(q1), q37_einkommen_systematik2, weighted = F, weight = gewicht)

# Kosten
q39_unterkunftskosten_median <- pp %>% filter(q39_unterkunftskosten > 0) %>% group_by(q1) %>% 
  summarise(across(c(q39_unterkunftskosten), ~median(.x, na.rm = T)))
q39_unterkunftskosten_median_n <- pp %>% filter(q39_unterkunftskosten > 0) %>% group_by(q1) %>% 
  summarise(across(c(q39_unterkunftskosten), ~sum(!is.na(.x)), .names = "{.col}_n"))

tab3_q39_unterkunftskosten_median <- q39_unterkunftskosten_median %>% left_join(q39_unterkunftskosten_median_n) %>% list() %>% set_names("q39_unterkunftskosten_median")

pp %>% filter(q39bb > 0) %>% 
  summarise(across(c(q39bb), ~median(.x, na.rm = T)))

# Übernahme
tabellenband_bivariat(pp %>% filter(q53 == "Ich werde mir einen Arbeitsplatz suchen."), c(q1), q37_einkommen_systematik2, weighted = F, weight = gewicht)

tab3_q52 <- tabellenband_bivariat(pp %>% filter(q53 == "Ich werde mir einen Arbeitsplatz suchen."), all_of(breaks[!breaks %in% c("q52")]), col_vars = c(q52), version = "summary", weighted = T, weight = gewicht) %>% list() %>% set_names("q52")

## Export
obj <- Filter(function(x) inherits(get(x), c("list")), ls())
obj <- obj[str_detect(obj, "tab3_")]

list_export <- c()

for (i in 1:length(obj)) {
  list_export <- c(list_export, get(obj[i]))
  print(obj[i])
}

write.xlsx(list_export, "pp_zusatzauswertungen.xlsx", overwrite = T)

write.xlsx(tab3_q1, "test.xlsx", overwrite = T)



## Todos

- Outputvariable erzeugen in einem Index

- neue Variablen in aktuellen DF mit alten abgleichen --> wie werden die weiterverwendet?
  new.labels <- pp[which(names(pp) %in% lbl.var.df$pp.variable == F),] %>% 
  get_label %>% 
  enframe(name= "pp.variable",
          value = "pp.label")

new.labels %>% 
  View




