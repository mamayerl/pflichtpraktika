# README ----
# 1) Auf der Website https://abc.berufsbildendeschulen.at/schoolfinder?kindOfSchool=berufsschulen&state=4 k?nnen die Schulen pro BL abgerufen werden.
# 2) den gesamten Seiteninhalt pro BL in Schustandorte.txt copy-pasten
# 3) mittels fnc.schulen Namen der Schulen extrahieren
library(tidyverse)
library(openxlsx)
library(tidyr)

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


# Abgleich alt-neu ----
schulnamen.df <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulnamen.df.RData")

schulnamen <- 
  schulnamen.df %>% 
  mutate(Straﬂe = str_replace_all(Straﬂe,"_", " "))

gesamtliste1 <- 
  gesamtliste %>% 
  mutate(Straﬂe = Adresse %>% str_remove(", \\d{4}.+"),
         Gemeinde = Adresse %>% str_remove(".+, \\d{4} "))

inner_join(schulnamen, gesamtliste1,
           by="Straﬂe",
           relationship = "many-to-many") %>% 
  distinct() %>% 
  View()
anti_join(schulnamen, gesamtliste1,
          by="Straﬂe") %>% 
  distinct()


neu <- fnc.excel.readallsheets("../Fragebogenentwicklung/Schulstandorte_neu.xlsx") %>% 
  list_rbind() %>%
  rename(Name = 2) %>%
  select(2) #%>%
  # unlist %>% 
  # as.vector
alt <- read.xlsx("../Fragebogenentwicklung/Schulstandorte_lm5.xlsx") %>% 
  as_tibble %>% 
  rename(Name = 2) %>%
  select(2) #%>%
  # unlist %>% 
  # as.vector

alt %>% 
  count(Code)

full_join(alt, neu, by = "Name")
which(alt %in% neu)

