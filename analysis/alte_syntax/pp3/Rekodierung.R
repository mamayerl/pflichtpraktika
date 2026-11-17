library(tidyverse)
library(haven)
library(psych)
library(sjlabelled)
library(sjmisc)
library(factoextra)
library(janitor)
library(lubridate)
library(ggplot2)
library(openxlsx)
library(flextable)
library(officer)

pp_dispens <- readRDS("pp_dispens.RData")
pp_origin <- readRDS("pp3.RData")
# pp <- readRDS("pp.RData")
pp <- readRDS("pp_bereinigt.RData")
lbl.var.df <- readRDS("Variablenabgleich_alt_neu.RData")
pp_tb <- readRDS("pp_tb.RData")
vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/R-Scripts/")
source(paste0(vorlagen.path,"functions_praktisch.R"), encoding = "latin1")
# pp2_tb <- readRDS("pp2/pp2_tb.RData") 
pp2 <- readRDS("pp2/pp2.RData")


pp.alt <- pp2
rm(pp2)
n <- ncol(pp_origin)
adress.liste <- read.xlsx("../frabo/ministerium_schulliste.xlsx", sheet = "oeibf_bearbeitet") %>%
  as_tibble
breaks_tab <- readRDS("breaks_tab.RData")

kat.vars <- lbl.var.df %>%
  filter(analytisch == T & var.typ == "Kategorisch") %>% 
  select(pp.variable) %>% 
  unique %>% 
  unlist %>% 
  as.vector

excl <- pp_tb %>%
  select(q12, matches("q14|q15|q35")) %>% # q14 und 15 enthalten keine Werte
  names() %>% 
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
  select(-strng) %>% 
  reduce(~ .x)

lbs.kat <- ft.label.fnc(vart = "Kategorisch") %>% 
  list_rbind() %>% 
  arrange(match(pp.variable, kat.vars))
lbs.ant <- ft.label.fnc(vart = "Anteil") %>% 
  list_rbind()
lbs.rati <- ft.label.fnc(vart = "Rating")
lbs.mehr <- ft.label.fnc(vart = "Mehrfach")

tbl.list <- readRDS("tbl_list.RData")
ft.list <- readRDS("ft_list.RData")
tbl.list.2gether <- readRDS("tbl_2gether.RData")

tbl.kat <- readRDS("tbl_kategorisch.RData")
tbl.ant <- readRDS("tbl_anteil.RData")
tbl.rati <- readRDS("tbl_rating.RData")
tbl.mehr <- readRDS("tbl_mehrfach.RData")

kat <- readRDS("ft_kategorisch.RData")
ant <- readRDS("ft_anteil.RData")
rati <- readRDS("ft_rating.RData")
mehr <- readRDS("ft_mehrfach.RData")

df <- pp_tb
df <- pp_tb %>% filter(Schule == unique(pp_tb$Schule)[j])
i <- 1
# var.vect <- mehr.vars[[1]]
var <- kat.vars[[i]]
var <- "q40_kosten_gesamt_kat"#kat.vars[i]
brk <- "Bundesland"


i <- 5
df <- tbl.kat[[5]] #%>% as_tibble()
frage = lbs.kat[i,] %>% select(Frage) %>% unique %>% unlist
variablename = lbs.kat[i,] %>% select(pp.variable) %>% unique %>% unlist
kategorie = lbs.ant[i,] %>% select(ma.kategorie) %>% unique %>% unlist

frage = lbs.mehr[[i]] %>% select(Frage) %>% unique %>% unlist
variablename = lbs.mehr[[i]] %>% select(pp.variable) %>% unique %>% unlist
kategorie = lbs.mehr[[i]] %>% select(ma.kategorie) %>% unique %>% unlist
wertebereich = " "
wb.max = 13

### letzter Stand: es stimmt was nicht bie flextable.mehrfach.fnc

vart = "Kategorisch|Mehrfach"


df <- pp#pp_tb
chained.labels = F
varnames <- kat.gg
varnames <- "q18_anz_bew_kat2"
chained.labels = F
out.labs = F
weighted = T
weight = "gewicht"

### figure function
df <- kat.fig[[2]][[1]] %>% ungroup
x.achse = sym("p")
y.achse = sym("var")
farbe = sym("f")
order.y = T
legends = T
rev.col.scale = T
title.true = T
ttl = sym("label.n")
legend.dir = "horizontal"
axis.text = T
col.width = 0.5
str.wrap.width = 50
title.wrap = 100
geom.text.size = 4
text.size =  14
axis.line.size = 0.15



#### Test ####

analyt = T 
vart = "Kategorisch"

lbl.var.df %>% 
  filter(pp.variable %>% str_detect("q10"))
  filter(pp.label %>% str_detect("erlassen"))

ft.list

pp_tb$q18_anz_bew_kat2 %>% frq

ft.list %>% names

tbl.list <- c(
  tbl.kat,
  tbl.ant,
  tbl.rati,
  tbl.mehr)

names(tbl.list)

keep(tbl.list, ~ .x[1,ncol(.)] < 5000) %>% names %>% cat(sep = '", "')

lbl.var.df %>% 
  filter(pp.variable %in% c("q04", "q25", "q52_1", "q52_2", "q52_3", "q40_kosten_gesamt_kat", "q51a", "q13", "q17", "q50", "q62"))

