library(janitor)
library(tidyverse)
library(sjmisc)
library(openxlsx)

pp3 <- readRDS("pp3.RData")

## Alle Zellenbesetzungen bei Kombination aus Schultyp und Bundesland kleiner gleich 5 entfernen
### Kombination mit <= 5 feststellen
filter.kombi <-
pp3 %>% 
  count(schultyp_rec, Bundesland, .drop=T) %>% 
  na.omit() %>% 
  filter(n <= 5)
saveRDS(filter.kombi, "Schultyp-Bundesland-kleiner-5.RData")

filter.kombi %>%
  print(n=Inf)

### Neuer DF mit gefiltereten Personen, die auf einer Filtervariable Wert == 1 erhalten. Dann an pp mit ID anknÃ¼pfen mit neuer filtervariable. Alle anderen haben dann T
temp.le5 <-
pp3 %>% 
  inner_join(filter.kombi %>% select(-n)) %>% 
  mutate(filter_gew = F) %>% 
  select(id, filter_gew)
pp.gew <- 
  pp3 %>% 
  left_join(temp.le5) %>% 
  mutate(filter_gew = if_else(filter_gew %>% is.na(), T, F)) %>% 
  filter(filter_gew)

### Prüfen
pp.gew %>% 
  count(schultyp_rec, Bundesland) %>% 
  na.omit() %>% 
  filter(n <= 5)

# saveRDS(filter.gew.id,"gewicht-filter.RData")


### Statcube brutto ####
# Daten aus Statcube einlesen:  Schulstatistik; Schüler:innen ab 2006; Schulstufen 10-13; alle Bundesländer
# notwendige Schultypen (im Suchfeld eingeben und als Spalte zuweisen):
# Fachschulen des Ausbildungsbereiches Mode
# Gewerbliche und technische Fachschulen (im engeren Sinn)
# Fachschulen des Ausbildungsbereiches Tourismus
# Fachschulen des Ausbildungsbereiches Kunstgewerbe
# Handelsschulen
# Dreijährige Fachschule für wirtschaftliche Berufe
# Fachschulen für Sozialberufe
# Mittlere Schulen für pädagogische Assistenzberufe
# Höhere technische und gewerbliche Lehranstalten (im engeren Sinn)
# Höhere Lehranstalten des Ausbildungsbereiches Mode
# Höhere Lehranstalten des Ausbildungsbereiches Tourismus
# Höhere Lehranstalten des Ausbildungsbereiches Kunstgewerbe
# Handelsakademien
# Höhere Lehranstalten für wirtschaftliche Berufe
# Sozialberufliche höhere Schulen
# Bildungsanstalten für Elementarpädagogik
# Bildungsanstalten für Sozialpädagogik

statcube <- read.xlsx("../datensatz/statcube_schuelerinnen_bl_form_klasse_2022_schuldetails.xlsx", sheet = "Tabelle1") %>% 
  as_tibble()

statcube.1 <-
statcube %>%
  fill(Bundesland) %>% 
  mutate(Schulstufe = Schulstufe %>% str_extract("\\d+") %>% as.numeric(),
         across(-c(1:2), ~ if_else(.x %>% str_detect("\\-"), NA, .x) %>% as.numeric)) %>% 
  pivot_longer(-c(Bundesland, Schulstufe), names_to = "genau") %>% 
  filter(Bundesland != "unbekannt") %>% 
  mutate(genau = genau %>% str_replace_all("\\."," "))
  
# Namen vergeben
statcube.11 <- statcube.1 %>%
  mutate(name = case_when(
    genau %>% str_detect("Elementarpäd") ~ "element.paed.bhs",
    genau %>% str_detect("Sozialpäd") ~ "sozial.paed.bhs",
    genau %>% str_detect("Handelsakademie") ~ "kaufm.bhs",
    genau %>% str_detect("Höhere.+[T-t]ourism") ~ "tourist.bhs",
    genau %>% str_detect("Höhere.+[W-w]irtsch") ~ "wirtschaft.bhs",
    genau %>% str_detect("Höhere.+Mode|Höhere.+[G-g]ewerb") ~ "techn.gewerb.bhs",
    genau %>% str_detect("Sozial.+höhere Schulen") ~ "sozial.bhs",
    genau %>% str_detect("Fachsc.+[G-g]ewerb|Fachsc.+Mode") ~ "techn.gewerb.bms",
    genau %>% str_detect("technische Fachschulen") ~ "techn.gewerb.bms",
    genau %>% str_detect("Handelsschule") ~ "kaufm.bms",
    genau %>% str_detect("Fachsc.+[W-w]irtschaft") ~ "wirtschaft.bms",
    genau %>% str_detect("Fachsc.+Sozialberufe") ~ "sozial.bms",
    genau %>% str_detect("pädag.+Assistenz") ~ "paedag.ass.bms",
    genau %>% str_detect("Fachsc.+[T-t]our") ~ "tourist.bms",
    )) %>% 
  # frq(genau)
  # left_join(namen, by = "genau") %>%
  relocate(name, .before = 2) %>%
  select(-genau)

## Abschlussklassen definieren 
statcube.2 <-
statcube.11 %>%
  mutate(filter = case_when(
    ### Prüfen: stimmen diese Kombinationen
    Schulstufe == 11 & name %>% str_detect("techn.+bms") ~ F,
    Schulstufe == 13 & name %>% str_detect("wirt.+bhs") ~ F,
    Schulstufe == 13 & name %>% str_detect("bhs") ~ T,
    Schulstufe == 12 & name %>% str_detect("techn.+bms|wirt.+bhs") ~ T,
    Schulstufe == 11 & name %>% str_detect("bms") ~ T,
    .default = F
    )) %>% 
  filter(filter) %>% 
  select(-filter) %>% 
  group_by(Bundesland, Schulstufe, name) %>%
  reframe(value = sum(value, na.rm = T)) %>%
  pivot_wider(names_from = "Bundesland", values_from = "value")


### Elementar- und sonderpädagogik müssen vereint werden, weil Sonderpäd ganz geringe Fälle nur hat
paedag.bhs <- 
statcube.2 %>% 
  filter(name %>% str_detect("paed\\.bhs")) %>% 
  mutate(across(-c(Schulstufe, name), ~ sum(.x)),
         name = "paedag.bhs") %>% 
  slice(1)

statcube.brutto <-
statcube.2 %>% 
  filter(!name %>% str_detect("paed\\.bhs")) %>% 
  bind_rows(paedag.bhs) %>% 
  select(-Schulstufe) %>% 
  arrange(name)

### prüfen, ob Abschlussklassen korrekt sind; jg wird später noch verwendet
jg <- 
statcube.2 %>% 
  select(Schulstufe, name) %>% 
  mutate(Jahrgang = case_when(Schulstufe == 11 ~ "3",
                              Schulstufe == 12 ~ "4",
                              Schulstufe == 13 ~ "5"))
jg


### PP brutto ####
# pp3$schultyp_rec %>% unique
pp.brutto <-
pp.gew %>%
  janitor::tabyl(schultyp_rec, Bundesland, show_na = F) %>% 
  mutate(schultyp_rec = case_when(schultyp_rec %>% str_detect("HAK") ~ "kaufm.bhs",
                                  schultyp_rec %>% str_detect("HAS") ~ "kaufm.bms",
                                  schultyp_rec %>% str_detect("HLW") ~ "wirtschaft.bhs",
                                  schultyp_rec %>% str_detect("HTL") ~ "techn.gewerb.bhs",
                                  schultyp_rec %>% str_detect("Fachschule.+wirtschaft") ~ "wirtschaft.bms",
                                  schultyp_rec %>% str_detect("Gewerb.+Fachschule") ~ "techn.gewerb.bms",
                                  schultyp_rec %>% str_detect("HLT") ~ "tourist.bhs",
                                  schultyp_rec %>% str_detect("Hotelfachschule") ~ "tourist.bms",
                                  schultyp_rec %>% str_detect("Fachschule.+Sozialberufe") ~ "sozial.bms",
                                  schultyp_rec %>% str_detect("pädag") ~ "paedag.bhs",
                                  # schultyp_rec %>% str_detect("Pädag") ~ "paedag.ass.bms", # wegen zu geringer Zellbesetzung entfernt und kommt auch nicht in statcube 2022 vor
                                  schultyp_rec %>% str_detect("Hotelfachschule") ~ "techn.gewerb.bms")) %>% 
  filter(schultyp_rec %>% is.na == F) %>% 
  rename(name = schultyp_rec) %>% 
  arrange(name) %>% 
  as_tibble()


### Gewichtung ####
# Verhältnis der Anteile der Zellen an Gesamtsumme zwischen statcube.brutto und pp.brutto 
gewicht.1 <- 
statcube.brutto %>% 
  pivot_longer(-1, names_to = "Bundesland", values_to = "GG") %>% 
  left_join(pp.brutto %>% 
              pivot_longer(-1, names_to = "Bundesland", values_to = "PP")) 
gewicht.1 %>% 
  print(n=Inf)
### Hier sollten keine fehlenden Werte sein.

gewicht <-
gewicht.1 %>% 
  mutate(p.GG = GG/sum(GG, na.rm = T),
         p.PP = PP/sum(PP, na.rm = T)) %>% 
  mutate(gewicht = p.GG/p.PP) %>% 
  mutate(schultyp_rec = case_when(
    name %>% str_detect("kaufm.bhs") ~        pp3$schultyp_rec %>% str_subset("HAK") %>% unique,
    name %>% str_detect("kaufm.bms") ~        pp3$schultyp_rec %>% str_subset("HAS") %>% unique,
    name %>% str_detect("wirtschaft.bhs") ~   pp3$schultyp_rec %>% str_subset("HLW") %>% unique,
    name %>% str_detect("techn.gewerb.bhs") ~ pp3$schultyp_rec %>% str_subset("HTL") %>% unique,
    name %>% str_detect("wirtschaft.bms") ~   pp3$schultyp_rec %>% str_subset("Fachschule.+wirtschaft") %>% unique,
    name %>% str_detect("techn.gewerb.bms") ~ pp3$schultyp_rec %>% str_subset("Gewerb.+Fachschule") %>% unique,
    name %>% str_detect("tourist.bhs") ~      pp3$schultyp_rec %>% str_subset("HLT") %>% unique,
    name %>% str_detect("tourist.bms") ~      pp3$schultyp_rec %>% str_subset("Hotelfachschule") %>% unique,
    name %>% str_detect("sozial.bms") ~       pp3$schultyp_rec %>% str_subset("Fachschule.+Sozialberufe") %>% unique,
    name %>% str_detect("paedag.bhs") ~       pp3$schultyp_rec %>% str_subset("pädag") %>% unique,
    # name %>% str_detect("paedag.ass.bms") ~   pp3$schultyp_rec %>% str_subset("Pädag") %>% unique,
    name %>% str_detect("techn.gewerb.bms") ~ pp3$schultyp_rec %>% str_subset("Hotelfachschule") %>% unique)) %>% 
  mutate(schultyp_rec2 = case_when(
    name %>% str_detect("kaufm.bhs") ~        paste0(pp3$schultyp_rec %>% str_subset("HAK") %>% unique(), " (", jg[jg$name == "kaufm.bhs", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("kaufm.bms") ~        paste0(pp3$schultyp_rec %>% str_subset("HAS") %>% unique(), " (", jg[jg$name == "kaufm.bms", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("wirtschaft.bhs") ~   paste0(pp3$schultyp_rec %>% str_subset("HLW") %>% unique(), " (", jg[jg$name == "wirtschaft.bhs", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("techn.gewerb.bhs") ~ paste0(pp3$schultyp_rec %>% str_subset("HTL") %>% unique(), " (", jg[jg$name == "techn.gewerb.bhs", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("wirtschaft.bms") ~   paste0(pp3$schultyp_rec %>% str_subset("Fachschule.+wirtschaft") %>% unique(), " (", jg[jg$name == "wirtschaft.bms", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("techn.gewerb.bms") ~ paste0(pp3$schultyp_rec %>% str_subset("Gewerb.+Fachschule") %>% unique(), " (", jg[jg$name == "techn.gewerb.bms", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("tourist.bhs") ~      paste0(pp3$schultyp_rec %>% str_subset("HLT") %>% unique(), " (", jg[jg$name == "tourist.bhs", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("tourist.bms") ~      paste0(pp3$schultyp_rec %>% str_subset("Hotelfachschule") %>% unique(), " (", jg[jg$name == "tourist.bms", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("sozial.bms") ~       paste0(pp3$schultyp_rec %>% str_subset("Fachschule.+Sozialberufe") %>% unique(), " (", jg[jg$name == "sozial.bms", "Jahrgang"] %>% pull(), ". JG)"),
    name %>% str_detect("paedag.bhs") ~       paste0(pp3$schultyp_rec %>% str_subset("pädag") %>% unique(), " (5. JG)"),
    # name %>% str_detect("paedag.ass.bms") ~   pp3$schultyp_rec %>% str_subset("Pädag") %>% unique,
    name %>% str_detect("techn.gewerb.bms") ~ pp3$schultyp_rec %>% str_subset("Hotelfachschule") %>% unique,
  ),
  schultyp_rec2 = str_replace(schultyp_rec2, "\\) \\(", "; "),
  .before = Bundesland) 

### prüfen
gewicht %>% 
  count(schultyp_rec2) %>% 
  print(n=Inf)
gewicht %>% 
  filter(gewicht > 10) %>% 
  print(n=Inf)


### Export Excel
gewicht %>% 
  select(schultyp_rec2, Bundesland, gewicht) %>% 
  mutate(gewicht = sprintf("%1.2f", gewicht),
         # 0/Zahl --> 0: diese Gruppe gibt es nicht in statcube aber in pp. Gewicht == 0
         # 0/0 --> NaN: diese Gruppe gibt es weder in pp noch in statcube und ist damit irrelevant
         # Zahl/0 --> Inf: hier gibt es keine Gewichtung, weil in PP keine Personen diese Kombination hatten.
         gewicht = case_when(gewicht == Inf ~ "k. St.",
                             gewicht == NaN ~ "Komb",
                             gewicht %>% is.na() ~ "k. GG",
                             .default = gewicht)) %>% 
  na.omit() %>% 
  pivot_wider(names_from = Bundesland, values_from = gewicht)
  # write.xlsx(file = "../bericht/Tabellen/Gewichtungstabelle.xlsx")

gewicht %>% 
  select(schultyp_rec2, Bundesland, p.GG) %>% 
  na.omit() %>% 
  pivot_wider(names_from = Bundesland, values_from = p.GG) %>% 
  write.xlsx(file = "test.xlsx")
system("open test.xlsx")

saveRDS(gewicht,"gewicht.RData")


## Kontrollrechnung in Excel
pp.b1 <- pp.brutto %>% 
  adorn_totals(c("row", "col"))
sc.b1 <- statcube.brutto %>% 
  adorn_totals(c("row", "col"))
brutto.list <- list(`Brutto pp mit Randhäufigkeit` = pp.b1,
                    `Brutto sc mit Randhäufigkeit` = sc.b1,
                    `Zelle < 6` = temp)
saveRDS(c(brutto.list,list(gewicht_R = gewicht.tab)), "brutto_liste.RData")
# mit einer Berechnung in Excel die GEwichte überprüfen
write.xlsx(c(brutto.list,list(gewicht_R = gewicht.tab)), file = "../bericht/Tabellen/Bruttolisten.xlsx") 

system("open ../bericht/Tabellen/Bruttolisten.xlsx")

