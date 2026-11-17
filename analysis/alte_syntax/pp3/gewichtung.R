library(janitor)
library(tidyverse)
library(sjmisc)
library(openxlsx)

pp3 <- readRDS("pp3.RData")

## Alle Zellenbesetzungen kleiner gleich 5 filtern.
filter.kombi <- pp3 %>% 
  group_by(schultyp_rec, Bundesland, .drop=F) %>% 
  reframe(n = n()) %>% 
  na.omit %>% 
  filter(n <= 5)
saveRDS(filter.kombi, "Schultyp-Bundesland-kleiner-5.RData")
# filter.kombi %>% 
#   View

### Neuer DF mit gefiltereten Personen, die auf einer Filtervariable Wert == 1 erhalten. Dann an pp mit ID anknÃ¼pfen mit neuer filtervariable. Alle anderen haben dann FALSE
temp <- pp3 %>% 
  inner_join(filter.kombi %>% select(-n)) %>% 
  mutate(filter_gew = F) %>% 
  select(id, filter_gew)
## Zellen sollten keine Besetzung < 5 beinhalten
# pp3 %>%
#   full_join(temp, by="id") %>%
#   mutate(filter_gew = ifelse(filter_gew %>% is.na, T, filter_gew)) %>%
#   filter(filter_gew == T) %>%
#   tabyl(schultyp_rec, Bundesland) %>%
#   View
## damit exportiere ich eine Filtervariable, die mittels "id" mit pp verknüpft werden kann.
filter.gew.id <- pp3 %>% 
  full_join(temp, by="id") %>% 
  mutate(filter_gew = ifelse(filter_gew %>% is.na, T, filter_gew)) %>%
  select(id, filter_gew) 
pp.gew <- pp3 %>% 
  left_join(filter.gew.id, by = "id") %>% 
  filter(filter_gew == T)
# saveRDS(filter.gew.id,"gewicht-filter.RData")



### Statcube brutto ####
# Daten aus Statcube einlesen:
statcube <- read.xlsx("../datensatz/statcube_schuelerinnen_bl_form_klasse_2022_schuldetails.xlsx", sheet = "Tabelle1") %>% 
  as_tibble()

statcube.1 <- statcube %>%
  fill(Bundesland) %>% 
  mutate(Schulstufe = Schulstufe %>% str_extract("\\d+") %>% as.numeric(),
         across(-c(1:2), ~ if_else(.x %>% str_detect("\\-"), NA, .x) %>% as.numeric)) %>% 
  pivot_longer(-c(Bundesland, Schulstufe), names_to = "genau") %>% 
  filter(Bundesland != "unbekannt") %>% 
  mutate(genau = genau %>% str_replace_all("\\."," "))
  
# # Namen vergeben
namen <- read.xlsx("../datensatz/statcube_schuelerinnen_bl_form_klasse_2022_schulgruppen.xlsx", sheet = "Schultypnamen") %>%
  as_tibble

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

statcube.2 <- statcube.11 %>%
  mutate(filter = case_when(
    Schulstufe == 11 & name %>% str_detect("techn.+bms") ~ F,
    Schulstufe == 13 & name %>% str_detect("wirt.+bhs") ~ F,
    Schulstufe == 13 & name %>% str_detect("bhs") ~ T,
    Schulstufe == 12 & name %>% str_detect("techn.+bms|wirt.+bhs") ~ T,
    Schulstufe == 11 & name %>% str_detect("bms") ~ T,
    .default = F
    )) %>% 
  filter(filter) %>% 
  # arrange(Bundesland, name) %>% 
  # View
  select(-filter) %>% 
  group_by(Bundesland, Schulstufe, name) %>%
  reframe(value = sum(value, na.rm = T)) %>%
  pivot_wider(names_from = "Bundesland", values_from = "value")


### Elementar- und sonderpädagogik müssen vereint werden
x <- statcube.2[statcube.2$name %>% str_which("paed\\.bhs"),-c(1,2)] %>% 
  colSums(na.rm = T)
statcube.brutto <- statcube.2 %>% 
  rbind(c(name = 13, Schultyp = "paedag.bhs",x)) %>% 
  filter(name %>% str_detect("paed\\.bhs", negate = T)) %>% 
  select(-1) %>% 
  filter(name != "sozial.bhs") %>% 
  filter(name != "paedag.ass.bms") %>% 
  arrange(name) %>% 
  mutate(across(-1, ~ as.numeric(.x)))

sum.sc <- statcube.brutto[-1] %>% sum

### PP brutto ####
# pp3$schultyp_rec %>% unique
pp.brutto <- pp.gew %>%
  tabyl(schultyp_rec, Bundesland, show_na = F) %>% 
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
  as_tibble

sum.pp <- pp.brutto[-1] %>% sum


### Gewichtung ####
# Verhältnis der Anteile der Zellen an Gesamtsumme zwischen statcube.brutto und pp.brutto 
gewicht.tab <- cbind(pp.brutto[1],
                     (statcube.brutto[,-1]/sum.sc)/(pp.brutto[,-1]/sum.pp)) %>%
                     # (pp.brutto[,-1]/statcube.brutto[,-1])) %>% 
  as_tibble() %>% 
  mutate(across(-1, ~ case_when(.x == Inf | .x %>% is.na ~ 0,
                                # .x == 0 ~ 1,
                                # .x %>% is.na ~ 0,
                                .default = .x))) 
saveRDS(gewicht.tab,"Gewichtungstabelle.RData")
gewicht.tab %>% 
  write.xlsx(file = "../bericht/Tabellen/Gewichtungstabelle.xlsx")
# 0/Zahl --> 0: diese Gruppe gibt es nicht in 2022 statcube aber in pp. Gewicht == 0
# 0/0 --> NaN: diese Gruppe gibt es weder in pp noch in statcube und ist damit irrelevant
# Zahl/0 --> Inf: hier gibt es keine Gewichtung, weil in PP keine Personen diese Kombination hatten. Daher sollte hier "0" stehen?
gewicht <- gewicht.tab %>%
  mutate(across(-1, ~ case_when(.x == Inf ~ 0,
                                # .x == 0 ~ 1,
                                # .x %>% is.na ~ 0,
                                .default = .x))) %>% 
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
    name %>% str_detect("techn.gewerb.bms") ~ pp3$schultyp_rec %>% str_subset("Hotelfachschule") %>% unique,
    )) %>% 
  select(-name) %>% 
  pivot_longer(-schultyp_rec, names_to = "Bundesland", values_to = "gewicht")
saveRDS(gewicht,"gewicht.RData")


pp.b1 <- pp.brutto %>% 
  adorn_totals(c("row", "col"))
sc.b1 <- statcube.brutto %>% 
  adorn_totals(c("row", "col"))
brutto.list <- list(`Brutto pp mit Randhäufigkeit` = pp.b1,
                    `Brutto sc mit Randhäufigkeit` = sc.b1,
                    `Zelle < 6` = temp)

saveRDS(c(brutto.list,list(gewicht_R = gewicht.tab)), "brutto_liste.RData")
write.xlsx(c(brutto.list,list(gewicht_R = gewicht.tab)), file = "../bericht/Tabellen/Bruttolisten.xlsx") # mit einer Berechnung in Excel die GEwichte überprüfen


## Kontrollrechnung
x <- (gewicht.tab[-1]*brutto.list[[1]][-11,-c(1,11)]) #%>% sum(na.rm = T)
x %>% 
  mutate(across(everything(), ~ if_else(.x %>% is.na, 0, .x)),
         perc = rowSums(., na.rm = T)/sum(., na.rm = T))

brutto.list[[2]][-11,-c(1,11)] %>% 
  mutate(perc = rowSums(., na.rm = T)/sum(., na.rm = T) %>% round(2)) %>% 
  as_tibble


### Rücklauf ####

pp <- readRDS("pp_tb.RData")
r <- data.frame(Schultyp = "Rücklauf", # das folgende Prozedere ist umständlich, weil aus anderem Skript übernommen
                (brutto.list$`Brutto pp mit Randhäufigkeit`[11,2:10]/brutto.list$`Brutto sc mit Randhäufigkeit`[11,2:10]*100) %>% 
                  mutate(across(where(is.numeric), ~ format(round(.x, 0), nsmall = 0, big.mark = ".", decimal.mark = ",") %>% paste0("%"))),
                Gesamt = "",
                Rückläufe = "") %>% 
  as_tibble
ruecklauf <- pp %>%
  tabyl(schultyp_rec.pp, Bundesland) %>% 
  filter(schultyp_rec.pp %>% str_detect("Assist", negate = T)) %>% 
  mutate(schultyp_rec.pp = schultyp_rec.pp %>% as.character) %>% 
  # adorn_totals(c("col"), name = "Gesamt") %>% 
  cbind(format(round(brutto.list$`Brutto pp mit Randhäufigkeit`[1:10,11]/brutto.list$`Brutto sc mit Randhäufigkeit`[1:10,11]*100, 
                     0), nsmall = 0, big.mark = ".", decimal.mark = ",") %>% 
          mutate(Total = paste0(Total,"%"))) %>% 
  rename(Schultyp = schultyp_rec.pp,
         Rückläufe = Total) %>%
  mutate(Gesamt = rowSums(.[2:10])) %>% 
  relocate(Rückläufe, .after = Gesamt) %>% 
  adorn_totals(c("row"), name = "Gesamt") %>%
  rbind(r) %>% 
  as_tibble 
ruecklauf[11:12, 11:12] <- ""

write.xlsx(ruecklauf, "../bericht/Tabellen/Rücklauf.xlsx")
