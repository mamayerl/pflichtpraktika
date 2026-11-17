library(tidyverse)
# library(kableExtra)
library(flextable)
library(sjmisc)
library(sjlabelled)
library(janitor)
library(officer)
library(ggplot2)

vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/")
source(paste0(vorlagen.path,"functions_praktisch.R"), encoding = "latin1")

pp_tb <- readRDS("pp_tb.RData")
breaks_tab <- readRDS("breaks_tab.RData")
lbl.var.df <- readRDS("Variablenabgleich_alt_neu.RData")

kat.vars <- lbl.var.df %>%
  filter(analytisch == T & var.typ == "Kategorisch") %>% 
  select(pp.variable) %>% 
  unique %>% 
  unlist %>% 
  as.vector
excl <- pp_tb %>%
  select(q12,matches("q14","q35")) %>% # q14 und 15 enthalten keine Werte
  names %>% 
  str_remove("_.+") %>% 
  paste0(collapse = "|")
kat.vars <- kat.vars[kat.vars %>% str_detect(excl, negate = T)]

ant.vars <- lbl.var.df %>%
  filter(analytisch == T & var.typ == "Anteil") %>% 
  # filter(pp.variable == "q18_anteil_antworten") %>% 
  select(pp.variable) %>% 
  unlist %>% 
  as.vector

rati.vars <- lbl.var.df %>%
  filter(analytisch == T & var.typ == "Rating") %>% 
  group_by(strng) %>%
  reframe(var = list(pp.variable)) %>% 
  select(-strng) %>% 
  reduce(~ .x)

mehr.vars <- lbl.var.df %>%
  filter(analytisch == T & var.typ == "Mehrfach") %>% 
  group_by(strng) %>%
  reframe(var = list(pp.variable)) %>% 
  distinct %>% 
  # arrange(strng) %>% 
  select(-strng) %>% 
  reduce(~ .x)


lbs.kat <- ft.label.fnc(vart = "Kategorisch") %>% 
  list_rbind() %>% 
  arrange(match(pp.variable, kat.vars))
lbs.ant <- ft.label.fnc(vart = "Anteil") %>% 
  list_rbind()
lbs.rati <- ft.label.fnc(vart = "Rating")
lbs.mehr <- ft.label.fnc(vart = "Mehrfach")


## Tabellen ####
### Kat
# tbl.kat <- list()
# # for(i in 3:4){
# for (i in 1:length(kat.vars)) {
#   tbl.kat[[i]] <- tbl.band.fnc(pp_tb, var = kat.vars[i], brk = breaks_tab, weighted = T, weight = "gewicht")
# }
# tbl.kat <- map(tbl.kat, ~.x[[1]])
# names(tbl.kat) <- kat.vars
# saveRDS(tbl.kat, "tbl_kategorisch.RData")
# 
# 
# ### Anteil
# tbl.ant <- map(ant.vars, ~ tbl.band.num.fnc(pp_tb, brk = breaks_tab, var = .x, weighted = T, weight = "gewicht"))
# 
# names(tbl.ant) <- ant.vars
# saveRDS(tbl.ant, "tbl_anteil.RData")
# 
# 
# ### Rating
# # fetch beginning and endpoint of scale
# # pp_tb %>% 
# #   select(all_of(map(rating.vars, ~ .x[1]) %>% unlist)) %>% 
# #   get_labels
# #   reframe(across(everything(), ~ max(.x, na.rm = T)))
# tbl.rati <- map(rati.vars, ~ tbl.band.num.fnc(pp_tb, brk = breaks_tab, var = .x, weighted = T, weight = "gewicht"))
# names(tbl.rati) <- map(rati.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# saveRDS(tbl.rati, "tbl_rating.RData")
# 
# ### Mehrfach
# tbl.mehr <- map(mehr.vars, ~
#                   tbl.band.fnc(pp_tb, var = .x, brk = breaks_tab, weighted = T, weight = "gewicht"))
# names(tbl.mehr) <- map(mehr.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# saveRDS(tbl.mehr, "tbl_mehrfach.RData")
#
# 
# 
# #### Tab Nachträglich ####
# tbl.kat <- readRDS("tbl_kategorisch.RData")
# tbl.ant <- readRDS("tbl_anteil.RData")
# tbl.rati <- readRDS("tbl_rating.RData")
# tbl.mehr <- readRDS("tbl_mehrfach.RData")
# names(tbl.kat) <- kat.vars
# names(tbl.ant) <- ant.vars
# names(tbl.rati) <- map(rati.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# names(tbl.mehr) <- map(mehr.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# 
# tbl.list <- list(tbl.kat = tbl.kat, 
#                  tbl.ant = tbl.ant, 
#                  tbl.rati = tbl.rati, 
#                  tbl.mehr = tbl.mehr)
# 
# ### q10: Betriebspraxis nur f?r Fachschulen
# x <- tbl.band.fnc(pp_tb %>% 
#                     filter(is.na(q02bms) == F), var = "q10", brk = breaks_tab, weighted = T, weight = "gewicht")
# x[[1]][4,1] <- "Schulform"
# x[[1]][15,1] <- "Geschlecht - Richtung"
# x[[1]] <- x[[1]] %>%
#   filter(Gesamt != 0) 
# tbl.list[[1]][names(tbl.list[[1]]) == "q10"] <- x
# 
# 
## q23 als Mehrfachantwort definieren
# x <- tbl.list$tbl.mehr[[which(names(tbl.list$tbl.mehr) == "q23")]]
# y <- reduce(map(x, ~ .x %>% select(Ja)), function(x,y) cbind(x,y))
# names(y) <- letters[1:length(y)]
# dims <- dim(x[[1]])
# out <- x[[1]] %>%
#   select(-c(3:(dims[2]-2))) %>% 
#   cbind(y) %>% 
#   as_tibble() %>% 
#   relocate(c(3,4), .after=last_col())
# tbl.list$tbl.mehr[[6]] <- out
# 
## Kleine Zellenbesetzungen entfernen: m?nnlich Soziales und P?dagogig
# for (i in names(tbl.list)) {
#   tbl.list[[i]] <- map(tbl.list[[i]], ~ .x %>% 
#                          filter(Kategorie %>% str_detect("Männlich \\- Pädagogisch|Männlich \\- Soziales", negate = T)) %>% 
#                          mutate(Fragestellung = if_else(Kategorie == "Weiblich - Pädagogisch", "Geschlecht - Richtung", Fragestellung)))
# }
# 
# ## mehrfachkategorien in tbl.mehr benennen
# for (i in 1:length(tbl.list$tbl.mehr)) {
#   names(tbl.list$tbl.mehr[[i]]) <- c("Fragestellung","Kategorie", lbs.mehr[[i]] %>% select(ma.kategorie) %>% unique %>% unlist %>% as.vector, "Gesamt","n (gültig)")
# }
# 
# for (i in 1:length(tbl.list$tbl.rati)) {
#   names(tbl.list$tbl.rati[[i]]) <- c("Fragestellung","Kategorie", lbs.rati[[i]] %>% select(ma.kategorie) %>% unique %>% unlist %>% as.vector, "Gesamt","n (gültig)")
# }
# 
# ### Speichern
# saveRDS(tbl.list, "tbl_list.RData")
# saveRDS(tbl.list[["tbl.kat"]], "tbl_kategorisch.RData")
# saveRDS(tbl.list[["tbl.ant"]], "tbl_anteil.RData")
# saveRDS(tbl.list[["tbl.rati"]], "tbl_rating.RData")
# saveRDS(tbl.list[["tbl.mehr"]], "tbl_mehrfach.RData")
# 
# tbl.list.2gether <- c(tbl.list[["tbl.kat"]],
#                       tbl.list[["tbl.ant"]],
#                       tbl.list[["tbl.rati"]],
#                       tbl.list[["tbl.mehr"]])
# 
# tbl.list.2gether <- tbl.list.2gether[names(tbl.list.2gether) %>% sort]
# saveRDS(tbl.list.2gether, "tbl_2gether.RData")
# 
# 
# 
# Flextables ####
# tbl.kat <- readRDS("tbl_kategorisch.RData")
# tbl.ant <- readRDS("tbl_anteil.RData")
# tbl.rati <- readRDS("tbl_rating.RData")
# tbl.mehr <- readRDS("tbl_mehrfach.RData")
# names(tbl.kat) <- kat.vars
# names(tbl.ant) <- ant.vars
# names(tbl.rati) <- map(rati.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# names(tbl.mehr) <- map(mehr.vars, ~ .x %>% str_extract(".+(?=_.+)") %>% unique)
# kat <- list()
# # for (i in 3:4){
# for (i in 1:length(tbl.kat)){
#   kat[[i]] <- flextable.kategorisch.fnc(tbl.kat[[i]],
#                                         frage = lbs.kat[i,] %>% select(Frage) %>% unique %>% unlist,
#                                         variablename = lbs.kat[i,] %>% select(newvar) %>% unique %>% unlist)
# }
# names(kat) <- kat.vars
# saveRDS(kat, "ft_kategorisch.RData")
# 
# 
# ### Anteil
# ant <- list()
# # for (i in 1){
# for (i in 1:length(tbl.ant)){
#   ant[[i]] <- flextable.anteil.fnc(tbl.ant[[i]],
#                                    frage = lbs.ant[i,] %>% select(Frage) %>% unique %>% unlist,
#                                    variablename = lbs.ant[i,] %>% select(newvar) %>% unique %>% unlist)
# }
# names(ant) <- ant.vars
# saveRDS(ant, "ft_anteil.RData")
# 
# 
# ### Rating
# rati <- list()
# for (i in 1:length(tbl.rati)){
#   rati[[i]] <- flextable.rating.fnc(tbl.rati[[i]],
#                                     frage = lbs.rati[[i]] %>% select(Frage) %>% unique %>% unlist,
#                                     kategorie = lbs.rati[[i]] %>% select(ma.kategorie) %>% unique %>% unlist,
#                                     variablename = lbs.rati[[i]] %>% select(newvar) %>% unique %>% unlist)
# }
# names(rati) <- map(rati.vars, ~ .x[1])
# saveRDS(rati, "ft_rating.RData")
# 
# 
# ### Mehrfach
# mehr <- list()
# # for (i in 1:1){
# for (i in 1:length(tbl.mehr)){
#   mehr[[i]] <- flextable.mehrfach.fnc(tbl.mehr[[i]],
#                                       frage = lbs.mehr[[i]] %>% select(Frage) %>% unique %>% unlist,
#                                       variablename = lbs.mehr[[i]] %>% select(newvar) %>% unique %>% unlist,
#                                       kategorie = lbs.mehr[[i]] %>% select(ma.kategorie) %>% unique %>% unlist)
# }
# names(mehr) <- map(mehr.vars, ~ .x[1])
# saveRDS(mehr, "ft_mehrfach.RData")
# 
# 
# 
# # kat <- readRDS("ft_kategorisch.RData")
# # ant <- readRDS("ft_anteil.RData")
# # rati <- readRDS("ft_rating.RData")
# # mehr <- readRDS("ft_mehrfach.RData")
# 
# names(kat) <- kat.vars
# names(ant) <- ant.vars
# names(rati) <- map(rati.vars, ~ .x[1])
# names(mehr) <- map(mehr.vars, ~ .x[1])
# # ft.list <- rati[5:8]
# # ft.list <- rati
# # ft.list <- num
# # ft.list <- mehr
# 
# ft.list <- c(kat,
#              ant,
#              rati,
#              mehr)
# 
# ### Namen in Reihenfolge gem?? FB anbringen
# ord <- readRDS("Zuordnung-LS-Gruppen-Fragen.RData")
# names(ft.list) <- names(ft.list) %>%
#   str_remove("_.+")
# 
# x <- names(ft.list)[names(ft.list) %in% ord$variable]
# for (i in x) {
#   names(ft.list)[names(ft.list) == i] <- ord[ord$variable == i, "newvar"]
# }
# 
# # Ordnung mit Zahlen nach Prefix als Ordnung anstatt F1, F10, F11 --> F1, F2, F3 ... F11
# order.ft <- ft.list %>%
#   names %>%
#   named.to.df.fnc() %>%
#   mutate(char = str_extract(label,"^[A-Z a-z]"),
#          num = str_extract(label, "(?<=^[A-Z a-z])\\d+") %>% as.integer,
#          char = if_else(num %>% is.na, NA, char)) %>%
#   arrange(char,num) %>%
#   select(-name)
# 
# ft.list <- ft.list[order.ft$label]
# 
# saveRDS(ft.list, "ft_list.RData")
# 
# 
# 
# 
# ### Flextab Nachträglich ####
# ### ToDo: Code in diesem Chunk k?nnte mit hIlfe von ft.list vereinheitlicht werden. Allerdings sollte man dann mit der Benennung der List-elements noch nachdenken, ob gem?? FB oder lt. pp
# tbl.list.2gether <- readRDS("tbl_2gether.RData")
# x <- lbl.var.df %>% # variablen, deren Flextables wegen zu gro?er Breite aufgeteilt werden m?ssen
#   filter(newvar %>% str_detect("D2|E1|F4|J4")) %>% 
#   distinct(strng) %>% 
#   unlist %>% 
#   paste0(collapse = "|")
# y <- tbl.list.2gether[which(names(tbl.list.2gether) %>% str_detect(x))] # Variablen aus tbl extrahieren
# lbl.var.df %>% # Liste mit alter und neuer Beziechnung; in richtige Reihenfolge bringen
#   filter(newvar %>% str_detect("D2|E1|F4|J4") & analytisch == T) %>% 
#   distinct(strng, newvar) %>% 
#   arrange(factor(strng, levels = names(y)))
# tbl.long <- list(
#   # q17 - D2: 6/5
#   y[[1]] %>% 
#     select(1:2, 3:8, (ncol(.)-1):(ncol(.))),
#   y[[1]] %>% 
#     select(1:2, 9:13, (ncol(.)-1):(ncol(.))),
#   # q23 - E1: 7/6
#   y[[2]] %>% 
#     select(1:2, 3:9, (ncol(.)-1):(ncol(.))),
#   y[[2]] %>% 
#     select(1:2, 10:15, (ncol(.)-1):(ncol(.))),
#   # q27 - F4: 9/8
#   y[[3]] %>% 
#     select(1:2, 3:11, (ncol(.)-1):(ncol(.))),
#   y[[3]] %>% 
#     select(1:2, 12:19, (ncol(.)-1):(ncol(.))),
#   # q62 - J4: 7/7
#   y[[4]] %>% 
#     select(1:2, 3:9, (ncol(.)-1):(ncol(.))),
#   y[[4]] %>% 
#     select(1:2, 10:16, (ncol(.)-1):(ncol(.)))
# )
# 
# # Labels und Fragen
# lbl.1 <- ft.label.fnc(vart = "Kategorisch|Mehrfach") 
# 
# names(lbl.1) <- map(lbl.1, ~ .x %>% select(pp.variable) %>% mutate(pp.variable = pp.variable %>% str_extract("^[a-z]\\d+")) %>% unique) %>% unlist
# 
# v <- lbl.1[names(lbl.1) %in% c("q27","q17","q23","q62")]
# v <- v[c("q27","q17","q23","q62")] # ordnen
# 
# lbs.long <- list(
#   ### Letzter Stand:
#   v[["q27"]] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
#   v[["q27"]] %>% mutate(newvar = paste0(newvar, " (Teil 2)")),# %>% unlist %>% as.vector,
#   v[["q17"]][1:6,] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
#   v[["q17"]][7:11,] %>% mutate(newvar = paste0(newvar, " (Teil 2)")),# %>% unlist %>% as.vector,
#   v[["q23"]][1:7,] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
#   v[["q23"]][8:13,] %>% mutate(newvar = paste0(newvar, " (Teil 2)")),# %>% unlist %>% as.vector,
#   v[["q62"]][1:7,] %>% mutate(newvar = paste0(newvar, " (Teil 1)")),# %>% unlist %>% as.vector,
#   v[["q62"]][8:14,] %>% mutate(newvar = paste0(newvar, " (Teil 2)"))# %>% unlist %>% as.vector,
# )
# 
# # Flextable Mehrfach
# long <- list()
# for (i in 3:length(tbl.long)){
#   long[[i]] <- flextable.mehrfach.fnc(tbl.long[[i]],
#                                       frage = lbs.long[[i]] %>% select(Frage) %>% unique %>% unlist,
#                                       variablename = lbs.long[[i]] %>% select(newvar) %>% unique %>% unlist,
#                                       kategorie = lbs.long[[i]] %>% select(ma.kategorie) %>% unique %>% unlist)
# }
# long <- long %>% 
#   compact
# names(long) <- map(lbs.long[3:8], ~ .x$newvar %>% unique)
# saveRDS(long, "ft_long_mehrfach.RData")
# # long <- readRDS("ft_long.RData")
# 
# # Flextable Kategorisch
# long.1 <- list()
# for (i in 1:2){
#   long.1[[i]] <- flextable.kategorisch.fnc(tbl.long[[i]],
#                                            frage = lbs.long[[i]] %>% select(Frage) %>% unique %>% unlist,
#                                            variablename = lbs.long[[i]] %>% select(newvar) %>% unique %>% unlist)
# }
# names(long.1) <- map(lbs.long[1:2], ~ .x$newvar %>% unique)
# saveRDS(long.1, "ft_long_kategorisch.RData")
# 
# # zusammenf?hren
# ft.list <- c(ft.list, long, long.1)
# ft.list <- ft.list[names(ft.list) %>% order]
# ft.list <- ft.list[-which(names(ft.list) %in% c("D2","E1","F4","J4"))]
# # neu2: 5.4.20
# 
# 
# ft.list[which(names(ft.list) == "F10")][[1]] <- flextable.kategorisch.fnc(tbl.list.2gether$q33_1, 
#                                                                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# Haben Sie mit Ihrem Praktikumsvertrag eine Vereinbarung zu den Bildungszielen des Lehrplans abgeschlossen?",
#                                                                           variablename = "F10_1")
# ft.list[which(names(ft.list) == "F10")][[2]] <- flextable.kategorisch.fnc(tbl.list.2gether$q33_2, 
#                                                                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# Haben Sie einen schriftlichen Arbeitsvertrag abgeschlossen bzw. Dienstzettel erhalten?",
#                                                                           variablename = "F10_2")
# ft.list[which(names(ft.list) == "F10")][[3]] <- flextable.kategorisch.fnc(tbl.list.2gether$q33_3, 
#                                                                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# Wurden Sie vom Betrieb bei der Sozialversicherung angemeldet (Sozial- und Unfallversicherung)?",
#                                                                           variablename = "F10_3")
# # kat$q33_1 <- flextable.kategorisch.fnc(tbl.kat$q33_1, 
# #                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# # Haben Sie mit Ihrem Praktikumsvertrag eine Vereinbarung zu den Bildungszielen des Lehrplans abgeschlossen?",
# #                           variablename = "F10")
# # kat$q33_2 <- flextable.kategorisch.fnc(tbl.kat$q33_2, 
# #                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# # Haben Sie einen schriftlichen Arbeitsvertrag abgeschlossen bzw. Dienstzettel erhalten?",
# #                           variablename = "F10")
# # kat$q33_3 <- flextable.kategorisch.fnc(tbl.kat$q33_3, 
# #                           frage = "Bitte bewerten Sie, ob folgende Aspekte zutreffen:
# # Wurden Sie vom Betrieb bei der Sozialversicherung angemeldet (Sozial- und Unfallversicherung)?",
# #                           variablename = "F10")
# 
# 
# ft.list[which(names(ft.list) %>% str_detect("F16"))][[1]] <- flextable.kategorisch.fnc(tbl.list.2gether$q38_einkommen_gesamt_kat, 
#                                                                                        frage = "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)",
#                                                                                        variablename = "F16_1")
# ft.list[which(names(ft.list) %>% str_detect("F16"))][[2]] <- flextable.kategorisch.fnc(tbl.list.2gether$q38_einkommen_kat, 
#                                                                                        frage = "Einkommen",
#                                                                                        variablename = "F16_2")
# ft.list[which(names(ft.list) %>% str_detect("F16"))][[3]] <- flextable.kategorisch.fnc(tbl.list.2gether$q38_taschengeld_kat, 
#                                                                                        frage = "Taschengeld",
#                                                                                        variablename = "F16_3")
# ft.list[which(names(ft.list) %>% str_detect("F16"))][[4]] <- flextable.kategorisch.fnc(tbl.list.2gether$q38_trinkgeld_kat, 
#                                                                                        frage = "Trinkgeld",
#                                                                                        variablename = "F16_4")
# ft.list[which(names(ft.list) %>% str_detect("F16"))][[5]] <- flextable.kategorisch.fnc(tbl.list.2gether$q38_einkommen_systematik2, 
#                                                                                        frage = "Verteilung der Einkommensarten",
#                                                                                        variablename = "F16_5")
# 
# 
# ft.list[which(names(ft.list) %>% str_detect("F18"))][[1]] <- flextable.kategorisch.fnc(tbl.list.2gether$q40_kosten_gesamt_kat, 
#                                                                                        frage = "Ausgaben gesamt (Arbeits-, Essens- und Fahrtkosten)",
#                                                                                        variablename = "F18_1")
# ft.list[which(names(ft.list) %>% str_detect("F18"))][[2]] <- flextable.kategorisch.fnc(tbl.list.2gether$q40_arbeitskosten_kat, 
#                                                                                        frage = "Arbeitskosten",
#                                                                                        variablename = "F18_2")
# ft.list[which(names(ft.list) %>% str_detect("F18"))][[3]] <- flextable.kategorisch.fnc(tbl.list.2gether$q40_essenskosten_kat, 
#                                                                                        frage = "Essenskosten",
#                                                                                        variablename = "F18_3")
# ft.list[which(names(ft.list) %>% str_detect("F18"))][[4]] <- flextable.kategorisch.fnc(tbl.list.2gether$q40_fahrtkosten_kat, 
#                                                                                        frage = "Fahrtkosten",
#                                                                                        variablename = "F18_4")
# 
# ft.list[which(names(ft.list) %>% str_detect("G8"))][[1]] <- flextable.kategorisch.fnc(tbl.list.2gether$q52_1,
#                                                                                       frage = "Wie genau haben Sie den Praktika-Wegweiser (www.praktika-bbs.at) eingesetzt?\nUm das Praktikum vorzubereiten",
#                                                                                       variablename = "G8_1")
# ft.list[which(names(ft.list) %>% str_detect("G8"))][[2]] <- flextable.kategorisch.fnc(tbl.list.2gether$q52_2,
#                                                                                       frage = "Wie genau haben Sie den Praktika-Wegweiser (www.praktika-bbs.at) eingesetzt?\nUm die Erfahrungen zu dokumentieren (z.B. Praktikumsblog, Praktikumsbericht)",
#                                                                                       variablename = "G8_2")
# ft.list[which(names(ft.list) %>% str_detect("G8"))][[3]] <- flextable.kategorisch.fnc(tbl.list.2gether$q52_3,
#                                                                                       frage = "Wie genau haben Sie den Praktika-Wegweiser (www.praktika-bbs.at) eingesetzt?\nUm das Praktikum nachzuweisen",
#                                                                                       variablename = "G8_3")
# 
# ft.list[[length(ft.list)]] <- flextable.kategorisch.fnc(tbl.list.2gether$schultyp_rec, 
#                                                         frage = "Schulform",
#                                                         variablename = "NA")
# 
# 
# 
# # footnotes f?r Teilstichproben
# ft.list[[which(names(ft.list) %>% str_detect("F2$"))]] <- ft.list[[which(names(ft.list) %>% str_detect("F2$"))]] %>% 
#   footnote(i = 4, j = 6, value = as_paragraph("Die Frage richtet sich nur an Personen mit einem Praktikumsplatz im Ausland (n=482)."), part = "header") %>%
#   fontsize(part = "footer", size = 8)
# 
# ft.list[[which(names(ft.list) %>% str_detect("C3$"))]] <- ft.list[[which(names(ft.list) %>% str_detect("C3$"))]] %>% 
#   footnote(i = 4, j = 7, value = as_paragraph("Die Frage richtet sich nur an Personen mit anteiligem Dispens (n=72)."), part = "header") %>%
#   fontsize(part = "footer", size = 8)
# 
# ft.list[[which(names(ft.list) %>% str_detect("F24$"))]] <- ft.list[[which(names(ft.list) %>% str_detect("F24$"))]] %>% 
#   fontsize(i = 3, j = 3:12, part = "header", size = 7) %>%
#   height(i = 3, height = 6.5, unit = "cm", part = "header")
# 
# ft.list[[which(names(ft.list) %>% str_detect("A9"))]] <- ft.list[[which(names(ft.list) %>% str_detect("A9"))]] %>%
#   footnote(i = 4, j = 8, value = as_paragraph("Die Frage richtet sich nur an Personen, die eine Fachschule besuchen (n=599)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# ft.list[[which(names(ft.list) %>% str_detect("B3"))]] <- ft.list[[which(names(ft.list) %>% str_detect("B3"))]] %>%
#   footnote(i = 4, j = 6, value = as_paragraph("Die Frage richtet sich nur an Fachschulschüler:innen (n=1.311). Entsprechend wurden leere Zeilen, die nur BHS-Sch?ler:innen betreffen, entfernt."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# 
# 
# ft.list[[which(names(ft.list) == "D2 (Teil 1)")]] <- ft.list[[which(names(ft.list) == "D2 (Teil 1)")]] %>%
#   footnote(i = 4, j = 9, value = as_paragraph("Die Frage richtet sich nur an Personen, f?r die es schwierig war, eine Praktikumsstelle zu finden (n=1.199)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# ft.list[[which(names(ft.list) == "D2 (Teil 2)")]] <- ft.list[[which(names(ft.list) == "D2 (Teil 2)")]] %>%
#   footnote(i = 4, j = 8, value = as_paragraph("Die Frage richtet sich nur an Personen, f?r die es schwierig war, eine Praktikumsstelle zu finden (n=1.199)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# ft.list[[which(names(ft.list) == "G6")]] <- ft.list[[which(names(ft.list) == "G6")]] %>%
#   footnote(i = 4, j = 4, value = as_paragraph("Die Frage richtet sich nur an Personen, die den PraktikaWegweiser verwendet haben (n=270)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# ft.list[[which(names(ft.list) == "J4 (Teil 1)")]] <- ft.list[[which(names(ft.list) == "J4 (Teil 1)")]] %>% 
#   delete_rows(part = "footer") %>% 
#   footnote(i = 4, j = 10, value = as_paragraph("Die Frage richtet sich nur an Personen, bei denen neben Deutsch eine andere Sprache daheim gesprochen wird (n=1.968)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# ft.list[[which(names(ft.list) == "J4 (Teil 2)")]] <- ft.list[[which(names(ft.list) == "J4 (Teil 2)")]] %>%
#   delete_rows(part = "footer") %>% 
#   footnote(i = 4, j = 10, value = as_paragraph("Die Frage richtet sich nur an Personen, bei denen neben Deutsch eine andere Sprache daheim gesprochen wird (n=1.968)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# 
# ft.list[[which(names(ft.list) == "G4")]] <- ft.list[[which(names(ft.list) == "G4")]] %>%
#   delete_rows(part = "footer") %>% 
#   footnote(i = 4, j = 9, value = as_paragraph("Die Frage richtet sich nur an Personen, bei denen Nachbesprechung stattgefunden hat (n=2.305)."), part = "header") %>% 
#   fontsize(size = 9, part = "footer")
# 
# 
# ### A9 löschen und mit Schultyp ersetzen
# ft.list$A9 <- ft.list$schultyp
# ft.list <- ft.list[-length(ft.list)]
# 
# saveRDS(ft.list, "ft_list.RData")
# 
#
#
# 
## Doc erstellen ####
ft.list <- readRDS("ft_list.RData")

## Word mit Tabellen erstellen
doc <- read_docx("Tabellenband_template.docx")
for (i in 1:23) {
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < 23) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband1.docx")


doc <- read_docx("Tabellenband_template.docx")
for (i in 24:47) {
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < 47) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband2.docx")


doc <- read_docx("Tabellenband_template.docx")
for (i in 48:length(ft.list)) {
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < length(ft.list)) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband3.docx")

saveRDS(doc, "ft_doc.RData")
print(doc, target = "Tabellenband3.docx")


doc <- read_docx("../bericht/Tabellenband/tabellenband_pp_einleitung.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband1.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband2.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband3.docx")

# print(doc, target = "Tabellenband_vorlaeufig.docx")
print(doc, target = "../bericht/Tabellenband/Tabellenband_vorlaeufig.docx")
system("open ../bericht/Tabellenband/Tabellenband_vorlaeufig.docx")