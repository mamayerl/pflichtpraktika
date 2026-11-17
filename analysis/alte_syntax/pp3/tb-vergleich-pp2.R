# Einlesen ----

library(tidyverse)
library(rstudioapi)
library(flextable)
library(sjmisc)
library(sjlabelled)
library(janitor)
library(officer)
library(openxlsx)

pp2_tb <- readRDS( file = "pp2/pp2_tb.RData")
pp2_tb <- pp2_tb %>% 
  mutate(q14_anz_bew_kat2 = if_else(q14_anz_bew_kat2 %>% str_detect("1 Bew|3 Bew|5 Bew"), "bis 5 Bewerbungen", q14_anz_bew_kat2) %>% 
           factor(levels = c("bis 5 Bewerbungen",
                             "6-10 Bewerbungen",
                             "11-20 Bewerbungen",
                             "mehr als 20 Bewerbungen")))

# breaks_tab <- readRDS("breaks_tab.RData")
vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/")
source(paste0(vorlagen.path,"functions_praktisch.R"), encoding = "latin1")
cb.pp2 <- pp2_tb %>% fnc.codebook()

# Variablen erkennen ----
# breaks_tab.vgl <- c(
#   "total",
#   "q01_rec",
#   "schultyp_rec",
#   "richtung_geschlecht",
#   "Bundesland$",
#   "betrieb",
#   "sprache",
#   "sozialer_hg",
#   "regional",
#   "pläne",
#   "bleiben",
#   "q36$",
#   "q31_ueberstunden",
#   "q55",
#   "cluster_qualitaet_3"
#                 )
# 

# pp2_tb <- pp2_tb %>% 
#   mutate(q37_einkommen_gesamt_kat = case_when(q37_einkommen_gesamt_kat %>% str_detect("1251|1501|1751|Mehr") ~ "Mehr als 1250 Euro",
#                                                   .default = q37_einkommen_gesamt_kat) %>% 
#            factor(levels = c("Kein Einkommen",
#                              "1-250 Euro",
#                              "251-500 Euro",
#                              "501-750 Euro",
#                              "751-1000 Euro",
#                              "1001-1250 Euro",
#                              "Mehr als 1250 Euro"))) 

# 
## Breaks: externe Bearbeitung ----
# brks.df.pp3 <- lbl.var.df %>% 
#   filter(pp.variable %>% str_detect(paste0(breaks_tab.vgl, collapse = "|"))) %>% 
#   filter(pp.variable %>% str_detect("_n|_total|_other", negate = T)) %>% 
#   mutate(Frage.fragment = str_extract(Frage, "\\b[:alpha:]+\\b \\b[:alpha:]+\\b"),
#          Frage.fragment = if_else(Frage.fragment %>% is.na, Frage, Frage.fragment),
#          filter = T)
# brks.df.pp3 %>% 
#   select(pp.variable, Frage) %>% 
#   write.xlsx("pp2/breaks_vgl_pp3.xlsx")
# system("open pp2/breaks_vgl_pp3.xlsx")
# 
# # Auslesen für die händische Zuordnung
# cb.pp2 %>% 
#   mutate(filter = if_else(Frage %>% str_detect("Bundesland|Unklarheit|Beschäftigung|Schultyp|stunden|richtung|Größe|Sprache|Bildungsabschluss|Wo haben|Pläne"), 1, 0)) %>% 
#   write.xlsx("pp2/Codebuch_pp2.xlsx")
# system("open pp2/Codebuch_pp2.xlsx")
# 
# cb.pp2 <- read.xlsx("pp2/Codebuch_pp2.xlsx") %>% 
#   as_tibble
# 
# brks.df.pp2 <- cb.pp2 %>% 
#   filter(filter == 1)
# saveRDS(brks.df.pp2, file = "pp2/brks.df.pp2.RData")
# brks.df.pp2 <- readRDS(file = "pp2/brks.df.pp2.RData")
breaks_tab <- c(
  "total",
  "q1_dich",
  "q1_rec",
  "richtung_geschlecht",
  "q4",
  "q28_rec",
  "q57a_rec",
  "q59_sozialer_hintergrund",
  "q24_rec",
  "q53_rec",
  "q27_rec",
  "q36",
  "q31_ueberstunden",
  "q51",
  "cluster_qualitaet"
)
  
## Variablen ----
# vars.pp3.a <- c("q16", # das sind die Variablen für den Vergleich
# "q17_",
# "g21_",
# "q31_ueberstunden",
# "q32",
# "q37",
# "q38_einkommen_gesamt_kat",
# "q23_",
# "q45_")
# 
# lbl.var.df %>% 
#   filter(pp.variable %>% str_detect(paste0(vars.pp3.a,collapse = "|"))) %>% 
#   filter(pp.variable %>% str_detect("_n|_total|_other|_unter", negate = T)) %>% 
#   mutate(filter = if_else(pp.variable %>% str_detect("_[2-9]([0-9]*)|_1[0-9]"), F, T)) %>% 
#   filter(filter) %>% 
#   write.xlsx("pp2/vars.df.pp3.xlsx")
# 
# cb.pp2 <- read.xlsx("pp2/Codebuch_pp2.xlsx") %>%
#   as_tibble
# vars.df.pp2 <- cb.pp2 %>%
#   filter(filter_vars == 1) 
# saveRDS(vars.df.pp2, file = "pp2/vars.df.pp2.RData")
# vars.df.pp2 <- readRDS(file = "pp2/vars.df.pp2.RData")
# cb.pp2 %>% 
#   filter(name %>% str_detect("q14_anz_bew_kat2"))
pp2.vars <- c("q12", "q13_1", "q13_2", "q13_3", "q13_4", "q13_5", "q13_6", "q13_7", "q14_anz_bew_kat2", "q19_1", "q19_2", "q19_3", "q19_4", "q19_5", "q19_6", "q19_7", "q19_8", "q19_9", "q22a_1", "q22a_2", "q22a_3", "q22a_4", "q22a_5", "q22a_6", "q22a_7", "q22a_8", "q22a_9", "q22a_10", "q22a_11", "q22a_12", "q22a_13", "q32", "q37a", "q42_1", "q42_2", "q42_3", "q42_4", "q42_5", "q42_6", "q42_7", "q42_8", "q42_9", "q42_10", "q31_ueberstunden", "q37_einkommen_gesamt_kat_rec")
vars.df.pp2 <- cb.pp2 %>% 
  filter(name %>% str_detect(pp2.vars %>% paste0("$", collapse = "|"))) %>% 
  mutate(var.typ = case_when(Frage %>% str_detect("Wie beurteilen Sie") ~ "Rating", 
                             name == "q37_einkommen_gesamt_kat_rec" ~ "Kategorisch",
                             name == "q31_ueberstunden" ~ "Kategorisch",
                             name == "q14_anz_bew_kat2" ~ "Kategorisch",
                             .default = var.typ),
         Frage = if_else(name %>% str_detect("q37"), "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)", Frage),
         name = if_else(name == "q37_einkommen_gesamt_kat", "q37_einkommen_gesamt_kat_rec",name))
  # bind_rows(tribble(~name, ~label, ~strng, ~ma.kategorie, ~Frage, ~analytisch, ~var.typ, ~newvar, ~filter, ~filter_vars,
  #       "q37_einkommen_gesamt_kat_rec", NA,"q37",NA,NA,TRUE,"Kategorisch",NA,0,1))

# vars.df.pp2 %>% names %>% cat(sep = ", ~")
# vars.df.pp2 %>% filter(name == "q37_einkommen_gesamt_kat") %>% unlist %>% cat(sep = ',')

kat.vars <- vars.df.pp2 %>% 
  filter(var.typ=="Kategorisch") %>% 
  select(name) %>% 
  unlist %>% 
  as.vector

mehr.vars <- vars.df.pp2 %>%
  filter(analytisch == T & var.typ == "Mehrfach") %>% 
  group_by(strng) %>%
  reframe(var = list(name)) %>% 
  distinct %>% 
  # arrange(strng) %>% 
  select(-strng) %>% 
  reduce(~ .x)

rati.vars <- vars.df.pp2 %>%
  filter(analytisch == T & var.typ == "Rating") %>% 
  group_by(strng) %>%
  reframe(var = list(name)) %>% 
  select(-strng) %>% 
  reduce(~ .x)


lbs.kat <- vars.df.pp2 %>%
  filter(var.typ=="Kategorisch") %>% 
  mutate(Frage = case_when(name == "q37_einkommen_gesamt_kat_rec"~ "Netto-Gehalt pro Monat", 
                           name == "q14_anz_bew_kat2" ~ "Anzahl Bewerbungen (kategorisiert)",
                           .default = Frage))
lbs.mehr <- vars.df.pp2 %>%
  filter(var.typ == "Mehrfach") %>% 
  mutate(string = strng) %>% 
  group_by(string) %>% 
  group_map(~.x)
lbs.rati <- vars.df.pp2 %>%
  filter(var.typ == "Rating")

lbs.kat$newvar <- c("2022 - G1","2022 - N3","2022 - P1","2022 - F2","2022 - N2","2022 - P2")
lbs.mehr[[1]]$newvar <- "2022 - G2"
lbs.mehr[[2]]$newvar <- "2022 - I3"
lbs.mehr[[3]]$newvar <- "2022 - K1"
lbs.rati$newvar <- "2022 - R3"

# Tabellen erzeugen ---- 
## Kat ----
tbl.kat <- list()
# for (i in 5) {
for (i in 1:length(kat.vars)) {
  tbl.kat[[i]] <- tbl.band.fnc(pp2_tb, var = kat.vars[i], brk = breaks_tab, weighted = T, weight = "gewicht")
}
tbl.kat <- map(tbl.kat, ~.x[[1]])
names(tbl.kat) <- kat.vars

### Einkommensvergleich ----
#### Mind. 35h Arbeitsstunden pro Woche für Einkommensvergleich:
tbl.kat[["q37_einkommen_gesamt_kat_rec"]] <- tbl.band.fnc(pp2_tb %>% filter(q31_ueberstunden %>% as.integer >= 3), var = "q37_einkommen_gesamt_kat_rec", brk = breaks_tab, weighted = T, weight = "gewicht")[[1]]

## Rating ----
tbl.rati <- map(rati.vars, ~ tbl.band.num.fnc(pp2_tb, brk = breaks_tab, var = .x, weighted = T, weight = "gewicht"))
names(tbl.rati) <- map(rati.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)

## Mehrfach ----
tbl.mehr <- map(mehr.vars, ~
                  tbl.band.fnc(pp2_tb, var = .x, brk = breaks_tab, weighted = T, weight = "gewicht"))
names(tbl.mehr) <- map(mehr.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)


## Nachtraeglich ----
tbl.list <- list(tbl.kat = tbl.kat, 
                 tbl.rati = tbl.rati, 
                 tbl.mehr = tbl.mehr)


## q22a als Mehrfachantwort definieren
x <- tbl.list$tbl.mehr[[which(names(tbl.list$tbl.mehr) == "q22a")]]
y <- reduce(map(x, ~ .x %>% select(Ja)), function(x,y) cbind(x,y))
names(y) <- letters[1:length(y)]
dims <- dim(x[[1]])
out <- x[[1]] %>%
  select(-c(3:(dims[2]-2))) %>% 
  cbind(y) %>% 
  as_tibble() %>% 
  relocate(c(3,4), .after=last_col())
tbl.list$tbl.mehr[[3]] <- out


## mehrfachkategorien in tbl.mehr benennen
for (i in 1:length(tbl.list$tbl.mehr)) {
  names(tbl.list$tbl.mehr[[i]]) <- c("Fragestellung","Kategorie", lbs.mehr[[i]] %>% select(ma.kategorie) %>% unique %>% unlist %>% as.vector, "Gesamt","n (gültig)")
}
## ratingkategorien in tbl.mehr benennen
for (i in 1:length(tbl.list$tbl.rati)) {
  names(tbl.list$tbl.rati[[i]]) <- c("Fragestellung","Kategorie", lbs.rati %>% select(ma.kategorie) %>% unique %>% unlist %>% as.vector, "n (min)")
}

tbl.list.2gether <- c(tbl.list[["tbl.kat"]],
                      tbl.list[["tbl.rati"]],
                      tbl.list[["tbl.mehr"]])
## "other" entfernen
# tbl.list$tbl.kat <- map(tbl.list$tbl.kat, ~ .x %>% select(-matches("^other$"))) 
# tbl.list$tbl.mehr <- map(tbl.list$tbl.mehr, ~ .x %>% select(-matches("^other$"))) 
# tbl.list$tbl.rati <- map(tbl.list$tbl.rati, ~ .x %>% select(-matches("^other$"))) 
# tbl.list.2gether <- tbl.list %>% flatten
# 
# saveRDS(tbl.list, file = "Vergleich-Zeit/tbl.list.pp2")

# Flextables ----
## kategorisch ----
kat <- list()
for (i in 1:length(tbl.list$tbl.kat)){
  kat[[i]] <- flextable.kategorisch.fnc(tbl.list$tbl.kat[[i]],
                                        frage = lbs.kat[i,] %>% select(Frage) %>% unique %>% unlist,
                                        variablename = lbs.kat[i,] %>% select(newvar) %>% unique %>% unlist)
  names(kat)[i] <- lbs.kat[i,] %>% select(newvar) %>% unique %>% unlist
}


## Rating ----
rati <- list()
for (i in 1:length(tbl.list$tbl.rati)){
  rati[[i]] <- flextable.rating.fnc(tbl.list$tbl.rati[[i]],
                                    frage = lbs.rati %>% select(Frage) %>% unique %>% unlist,
                                    kategorie = lbs.rati %>% select(ma.kategorie) %>% unique %>% unlist,
                                    variablename = lbs.rati %>% select(newvar) %>% unique %>% unlist)
  names(rati)[i] <- lbs.rati[1,] %>% select(newvar) %>% unique %>% unlist
}


## Mehrfach ----
mehr <- list()
# for (i in 1:1){
for (i in 1:length(tbl.list$tbl.mehr)){
  mehr[[i]] <- flextable.mehrfach.fnc(tbl.list$tbl.mehr[[i]],
                                      frage = lbs.mehr[[i]] %>% select(Frage) %>% unique %>% unlist,
                                      variablename = lbs.mehr[[i]] %>% select(newvar) %>% unique %>% unlist,
                                      kategorie = lbs.mehr[[i]] %>% select(ma.kategorie) %>% unique %>% unlist)
  names(mehr)[i] <- lbs.mehr[[i]][1,] %>% select(newvar) %>% unique %>% unlist
}


## nachtraeglich ----
# neu1: 5.4.2024: Tabellen, die zu breit sind, in Teil-tabellen trennen und urspr?ngliche ersetzen.
x <- cb.pp2 %>% # variablen, deren Flextables wegen zu gro?er Breite aufgeteilt werden m?ssen
  filter(name %>% str_detect("22a_\\d+$|42_\\d+$")) %>% 
  distinct(strng) %>% 
  unlist %>% 
  paste0(collapse = "|")
y <- tbl.list.2gether[which(names(tbl.list.2gether) %>% str_detect(x))] # Variablen aus tbl extrahieren
y <- y[names(y) %>% sort] # Liste mit alter und neuer Beziechnung; in richtige Reihenfolge bringen
tbl.long <- list(
  # q17 - D2: 6/5
  y[[1]] %>% 
    select(1:2, 3:9, (ncol(.)-1):(ncol(.))),
  y[[1]] %>% 
    select(1:2, 10:15, (ncol(.)-1):(ncol(.))),
  # q23 - E1: 7/6
  y[[2]] %>% 
    select(1:2, 3:7, (ncol(.)):(ncol(.))),
  y[[2]] %>% 
    select(1:2, 8:12, (ncol(.)):(ncol(.)))
)
# names.long <- c("q22a","q42")
# names(tbl.long) <- names.long

# Labels und Fragen
lbs.long <- list(
  lbs.mehr[[3]][1:6,] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
  lbs.mehr[[3]][7:11,] %>% mutate(newvar = paste0(newvar, " (Teil 2)")),# %>% unlist %>% as.vector,
  lbs.rati[1:5,] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
  lbs.rati[6:10,] %>% mutate(newvar = paste0(newvar, " (Teil 2)"))# %>% unlist %>% as.vector,
)

# Flextable Mehrfach
long.mehrfach <- list()
for (i in 1:2){
  long.mehrfach[[i]] <- flextable.mehrfach.fnc(tbl.long[[i]],
                                      frage = lbs.long[[i]] %>% select(Frage) %>% unique %>% unlist,
                                      variablename = lbs.long[[i]] %>% select(newvar) %>% unique %>% unlist,
                                      kategorie = lbs.long[[i]] %>% select(ma.kategorie) %>% unique %>% unlist)
}
long.mehrfach <- long.mehrfach %>% 
  compact
names(long.mehrfach) <- map(lbs.long, ~ .x$newvar %>% unique)[1:2]


# # Flextable rating
long.rating <- list()
for (i in 3:4){
  long.rating[[i]] <- flextable.rating.fnc(tbl.long[[i]],
                                               frage = lbs.long[[i]] %>% select(Frage) %>% unique %>% unlist,
                                               variablename = lbs.long[[i]] %>% select(newvar) %>% unique %>% unlist,
                                               kategorie = lbs.long[[i]] %>% select(ma.kategorie) %>% unique %>% unlist)
}
long.rating <- long.rating %>% 
  compact
names(long.rating) <- map(lbs.long, ~ .x$newvar %>% unique)[3:4]


# zusammenf?hren
# ft.list <- c(ft.list, long, long.1)
ft.list <- c(kat, rati, mehr, long.mehrfach, long.rating)
ft.list <- ft.list[-which(names(ft.list) %in% c("2022 - R3","2022 - K1"))]

ft.list <- ft.list[c(
  "2022 - G1", # schweirig
  "2022 - G2", # warum schwierig
  "2022 - F2", # Bewerbungen
  "2022 - I3", # Suche wichtig
  "2022 - N2", # Arbeitszeit
  "2022 - N3", # Wochenende
  "2022 - P1", # Bezahlung erhalten
  "2022 - P2", # Höhe einkommen
  "2022 - K1 (Teil 1)", # schul. unterstützung
  "2022 - K1 (Teil 2)",
  "2022 - R3 (Teil 1)", # Rückblickend
  "2022 - R3 (Teil 2)"
)]

ft.list <- ft.list[names(ft.list) %>% order]

ft.list[[which(names(ft.list) == "2022 - G2")]] <- ft.list[[which(names(ft.list) == "2022 - G2")]] %>%
  footnote(i = 4, j = 9, value = as_paragraph("Die Frage richtet sich nur an Personen, für die es schwierig war, eine Praktikumsstelle zu finden (n=1.560)."), part = "header") %>% 
  fontsize(size = 9, part = "footer")
ft.list[[which(names(ft.list) == "2022 - P2")]] <- ft.list[[which(names(ft.list) == "2022 - P2")]] %>%
  footnote(i = 4, j = 11, value = as_paragraph("Die Frage richtet sich nur an Personen mit einer Wochenarbeitszeit von über 35 Stunden. Es wurde keine Inflationsbereinigung durchgeführt."), part = "header") %>% 
  fontsize(size = 9, part = "footer")


saveRDS(tbl.list, file = "Vergleich-Zeit/tb-vergleich-tbl-list-pp2.RData")
saveRDS(ft.list, file = "Vergleich-Zeit/tb-vergleich-ft-list-pp2.RData")


# Docs Erzeugen ----
## Word mit Tabellen erstellen
doc <- read_docx("Tabellenband_template.docx")

for (i in 1:length(ft.list)) {
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < length(ft.list)) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Vergleich-Zeit/Tabellenband-Vergleich-Zeit-pp2.docx")
system("open Vergleich-Zeit/Tabellenband-Vergleich-Zeit-pp2.docx")

