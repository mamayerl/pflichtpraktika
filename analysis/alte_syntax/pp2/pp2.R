library(tidyverse)
library(haven)
library(sjlabelled)
vorlagen.path <- paste0(getwd() %>% str_remove("(?<=\\/data\\/).+"), "8_Mitarbeiterordner/Georg/Vorlagen/")
source(paste0(vorlagen.path,"functions_praktisch.R"), encoding = "latin1")
pp2_origin <- read_sav("pp2/pp2_daten_final_gewichtet.sav")
pp2_origin <- forcats::as_factor(pp2_origin)

## Teile Datensatz in jene, die Praktikum absolviert haben und jene mit Dispens

pp2 <- pp2_origin %>% filter(q6 != "wurde mir komplett erlassen (Dispens).")
pp2_dispens <- pp2_origin %>% filter(q6 == "wurde mir komplett erlassen (Dispens).")


pp2_dispens <- pp2_dispens %>% mutate(q8 = fct_recode(q8, "Ja" = "other"), 
                                    q10_1_kat = cut(q10_1, breaks = c(-1, 3, 5, 10, 20, 200), 
                                                    labels = c("bis 3 Bewerbungen", "4-5", "6-10", "11-20", "mehr als 20 Bewerbungen")), 
                                    q10_anteil_antworten = (q10_2 / q10_1) * 100, 
                                    q10_corona_absage = (q11 / q10_2) * 100)

## MC - q9
pp2_dispens <- pp2_dispens %>% mutate(q9_other2 = if_else(q9_other != "", "Ja", NA_character_), 
                                    q9_other2 = factor(q9_other2, levels = c("Nicht Gewählt", "Ja")))
pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7), ~fct_drop(.x, only = "other")))
pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7, q9_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
#pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7, q9_other2), ~fct_relevel(.x, "Nicht Gewählt")))
pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7, q9_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2_dispens <- pp2_dispens %>% rowwise() %>% mutate(q9_total = sum(c_across(c(q9_1_n:q9_7_n, q9_other2_n)))) %>% ungroup()
pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7, q9_other2), ~as.character(.x)))
pp2_dispens <- pp2_dispens %>% mutate(across(c(q9_1:q9_7, q9_other2), ~if_else(q9_total == 0, NA_character_ , .x)))

pp2_dispens <- copy_labels(pp2_dispens, pp2_origin)


pp2_origin <- pp2_origin %>% mutate(q59_sozialer_hintergrund = fct_collapse(q59, 
                                                                          "niedrig" = "Pflichtschule (Hauptschule, Neue Mittelschule, Poly, PTS etc.)", 
                                                                          "mittel" = c("Lehre, Berufsschule", "Berufsbildende mittlere Schule (Fachschule, Handelsschule, Sozialbetreuung, Gesundheits- und Krankenpflege etc.)"), 
                                                                          "gehoben" = c("Matura (Gymnasium, HTL, HAK, HBLA, BAKIP/BAfEP), Berufsreifeprüfung", "Meisterprüfung, Werkmeister"), 
                                                                          "hoch" = c("Universität, Fachhochschule, Pädagogische Hochschule, Akademie")))

## Variablen Vorbereiten
pp2 <- pp2 %>% mutate(q1_schulform = fct_collapse(q1, 
                                                "Kaufmännisch" = c("Handelsakademie (HAK)", "Handelsschule (HAS)"), 
                                                "Technisch-(kunst)gewerblich" = c("Höhere technische und gewerbliche (einschlieÃŸlich kunstgewerblicher) Lehranstalten (HTL)", "Gewerbliche, technische und kunstgew. Fachschulen"), 
                                                "Wirtschaftlich" = c("Höhere Lehranstalten für wirtschaftliche Berufe (HLW)", "Fachschule für wirtschaftliche Berufe"), 
                                                "Touristisch" = c("Höhere Lehranstalt für Tourismus (HLT)", "Hotelfachschule, Tourismusfachschule")),
                    q1_schultyp = fct_collapse(q1, 
                                               "BMS" = c("Handelsschule (HAS)", "Gewerbliche, technische und kunstgew. Fachschulen", "Fachschule für wirtschaftliche Berufe", "Hotelfachschule, Tourismusfachschule"), 
                                               "BHS" = c("Handelsakademie (HAK)", "Höhere technische und gewerbliche (einschlieÃŸlich kunstgewerblicher) Lehranstalten (HTL)", "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)", "Höhere Lehranstalt für Tourismus (HLT)")),
                    q14_anz_bew = if_else(q14_1 > 300, NA_real_, q14_1), 
                    q14_anz_bew_kat = cut(q14_anz_bew, breaks = c(-1, 0, 1, 3, 5, 10, 15, 20, 30, 50, 100, 301), 
                                          labels = c("Keine Bewerbung", "1 Bewerbung", "2-3 Bewerbungen", "4-5 Bewerbungen", "6-10 Bewerbungen", "11-15 Bewerbungen", "16-20 Bewerbungen", "21-30 Bewerbungen", "31-50 Bewerbungen", "51-100 Bewerbungen", "Mehr als 100 Bewerbungen")), 
                    
                    
                    
                    q14_anz_bew_kat2 = cut(q14_anz_bew, breaks = c(-1, 1, 3, 5, 10, 20, 200), 
                                           labels = c("bis 1 Bewerbung", "2-3 Bewerbungen", "4-5 Bewerbungen", "6-10 Bewerbungen", "11-20 Bewerbungen", "mehr als 20 Bewerbungen")),
                    q14_anteil_antworten = (q14_2 / q14_1) * 100, 
                    q14_corona_absagegrund = (q14a / q14_2) * 100,
                    q31_ueberstunden = cut(q31, breaks = c(-1, 20, 35, 40, 61), labels = c("bis 20 Stunden", "21-35 Stunden", "36-40 Stunden", "Mehr als 40 Stunden")),
                    q58_notendurchschnitt = (as.numeric(q58_1) + as.numeric(q58_2) + as.numeric(q58_3))/3,
                    q58_notendurchschnitt = cut(q58_notendurchschnitt, breaks = c(-1, 2, 3, 4, 5, 6.01), labels = c("Sehr gut", "Gut", "Befriedigend", "Genügend", "Nicht Genügend")),
                    q59_sozialer_hintergrund = fct_collapse(q59, 
                                                            "niedrig" = "Pflichtschule (Hauptschule, Neue Mittelschule, Poly, PTS etc.)", 
                                                            "mittel" = c("Lehre, Berufsschule", "Berufsbildende mittlere Schule (Fachschule, Handelsschule, Sozialbetreuung, Gesundheits- und Krankenpflege etc.)"), 
                                                            "gehoben" = c("Matura (Gymnasium, HTL, HAK, HBLA, BAKIP/BAfEP), Berufsreifeprüfung", "Meisterprüfung, Werkmeister"), 
                                                            "hoch" = c("Universität, Fachhochschule, Pädagogische Hochschule, Akademie"))) 

## Einkommen bereinigen
pp2 <- pp2 %>% mutate(q37_einkommen = if_else(q37_1 > 5000, NA_real_, q37_1),
                    q37_einkommen = if_else(q37a == "Nein", 0, q37_einkommen),
                    q37_taschengeld = if_else(q37_2 > 2000, NA_real_, q37_2),
                    q37_taschengeld = if_else(q37a == "Nein", 0, q37_taschengeld),
                    q37_trinkgeld = if_else(q37_3 > 2000, NA_real_, q37_3),
                    q37_trinkgeld = if_else(q37a == "Nein", 0, q37_trinkgeld), 
                    q37_einkommen = if_else(!is.na(q37_taschengeld) & is.na(q37_einkommen), 0, q37_einkommen), 
                    q37_taschengeld = if_else(!is.na(q37_einkommen) & is.na(q37_taschengeld), 0, q37_taschengeld), 
                    q37_trinkgeld = if_else(is.na(q37_trinkgeld), 0, q37_trinkgeld), 
                    q37_trinkgeld = if_else(is.na(q37_einkommen) & is.na(q37_taschengeld) & is.na(q37_3), NA_real_, q37_trinkgeld), 
                    q37_eink_gesamt = q37_einkommen + q37_taschengeld, 
                    q37_einkommen_kat = cut(q37_einkommen, breaks = c(-1, 0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 5001), labels = c("Kein Einkommen", "1-250 Euro","251-500 Euro", "501-750 Euro", "751-1000 Euro", "1001-1250 Euro", "1251-1500 Euro", "1501-1750 Euro", "1751-2000 Euro", "Mehr als 2000 Euro")),
                    q37_taschengeld_kat = cut(q37_taschengeld, breaks = c(-1, 0, 250, 500, 750, 1000, 2001), 
                                              labels = c("Kein Taschengeld", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro", "Mehr als 1000 Euro")),
                    q37_trinkgeld_kat = cut(q37_trinkgeld, breaks = c(-1, 0, 250, 500, 750, 1000, 2001), labels = c("Kein Trinkgeld", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro", "Mehr als 1000 Euro")),
                    q37_einkommen_gesamt_kat = cut(q37_eink_gesamt, breaks = c(-1, 0, 250, 500, 750, 1000, 1250, 1500, 1750, 2000, 5001), labels = c("Kein Einkommen", "1-250 Euro","251-500 Euro", "501-750 Euro", "751-1000 Euro", "1001-1250 Euro", "1251-1500 Euro", "1501-1750 Euro", "1751-2000 Euro", "Mehr als 2000 Euro")), 
                    q37_einkommen_systematik = if_else(q37_einkommen > 0 & q37_taschengeld > 0 & q37_trinkgeld > 0, "Einkommen + Taschengeld + Trinkgeld", 
                                                       if_else(q37_einkommen > 0 & q37_taschengeld > 0 & q37_trinkgeld == 0, "Einkommen + Taschengeld", 
                                                               if_else(q37_einkommen > 0 & q37_taschengeld == 0 & q37_trinkgeld > 0, "Einkommen + Trinkgeld", 
                                                                       if_else(q37_einkommen == 0 & q37_taschengeld > 0 & q37_trinkgeld > 0, "Taschengeld + Trinkgeld", 
                                                                               if_else(q37_einkommen > 0 & q37_taschengeld == 0 & q37_trinkgeld == 0, "Einkommen", 
                                                                                       if_else(q37_einkommen == 0 & q37_taschengeld > 0 & q37_trinkgeld == 0, "Taschengeld", 
                                                                                               if_else(q37_einkommen == 0 & q37_taschengeld == 0 & q37_trinkgeld > 0, "Trinkgeld", 
                                                                                                       if_else(q37_einkommen == 0 & q37_taschengeld == 0 & q37_trinkgeld == 0, "Keines", "Sonstiges")))))))), 
                    q37_einkommen_systematik = factor(q37_einkommen_systematik, levels = c("Einkommen", "Einkommen + Trinkgeld", "Einkommen + Taschengeld", "Einkommen + Taschengeld + Trinkgeld", "Taschengeld", "Taschengeld + Trinkgeld", "Trinkgeld", "Keines
")),
                    q37_einkommen_systematik2 = if_else(q37_einkommen == 0 & q37_taschengeld == 0, "Kein Einkommen", 
                                                        if_else(q37_einkommen > 0 & q37_taschengeld == 0, "Einkommen", 
                                                                if_else(q37_einkommen == 0 & q37_taschengeld > 1, "Taschengeld", 
                                                                        if_else(q37_einkommen > 0 & q37_taschengeld > 0, "Einkommen + Taschengeld", "Sonstiges"))))
)

# Kosten berechnen
pp2 <- pp2 %>% mutate(q39_unterkunftskosten = if_else(is.na(q39_1), 0, q39_1),
                    q39_unterkunftskosten = if_else(q39_unterkunftskosten >= 1000, NA_real_, q39_unterkunftskosten),
                    q39_fahrtkosten = if_else(q39_2 >= 500, NA_real_, q39_2), 
                    q39_essenskosten = if_else(q39_3 >= 500, NA_real_, q39_3),
                    q39_unterkunftskosten = if_else(is.na(q37_einkommen) & is.na(q37_taschengeld) & is.na(q37_trinkgeld), NA_real_, q39_unterkunftskosten),
                    q39_fahrtkosten = if_else(is.na(q37_einkommen) & is.na(q37_taschengeld) & is.na(q37_trinkgeld), NA_real_, q39_fahrtkosten),
                    q39_essenskosten = if_else(is.na(q37_einkommen) & is.na(q37_taschengeld) & is.na(q37_trinkgeld), NA_real_, q39_essenskosten),
                    across(c(q39_unterkunftskosten, q39_fahrtkosten, q39_essenskosten), ~if_else(.x < 0, NA_real_, .x)), 
                    q39_arbeitskosten = if_else(q39a == "Nein", 0, q39aa),
                    q39_kosten_gesamt = q39_unterkunftskosten + q39_fahrtkosten + q39_essenskosten,
                    q39_essenskosten_kat = cut(q39_essenskosten, breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
                    q39_fahrtkosten_kat = cut(q39_fahrtkosten,  breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
                    q39_arbeitskosten_kat = cut(q39_arbeitskosten, breaks = c(-1, 0, 50, 100, 200, 500), labels = c("Keine Kosten", "1-50 Euro", "51-100 Euro", "101-200 Euro", "Mehr als 200 Euro")),
                    q39_kosten_gesamt_kat = cut(q39_kosten_gesamt, breaks = c(-1, 0, 250, 500, 750, 1001), labels = c("Keine Kosten", "1-250 Euro", "251-500 Euro", "501-750 Euro", "751-1000 Euro")),
                    einkommen_kosten = q37_eink_gesamt - q39_kosten_gesamt)


# Multiple Choice Items rekodieren.
## q13 
pp2 <- pp2 %>% mutate(q13_other2 = if_else(q13_other != "", "Ja", NA_character_), 
                    q13_other2 = factor(q13_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q13_1:q13_7), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q13_1:q13_7, q13_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q13_1:q13_7, q13_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q13_total = sum(c_across(c(q13_1_n:q13_7_n, q13_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q13_1:q13_7, q13_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q13_1:q13_7, q13_other2), ~if_else(q13_total == 0 | (q12 == "Sehr leicht" | q12 == "Leicht"), NA_character_ , .x)))

## q17
pp2 <- pp2 %>% mutate(across(c(q17_1:q17_3), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q17_1:q17_3), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q17_1:q17_3), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q17_total = sum(c_across(c(q17_1_n:q17_3_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q17_1:q17_3), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q17_1:q17_3), ~if_else(q17_total == 0, NA_character_ , .x)))

## q19
pp2 <- pp2 %>% mutate(q19_other2 = if_else(q19_other != "", "Ja", NA_character_), 
                    q19_other2 = factor(q19_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q19_1:q19_9), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q19_1:q19_9, q19_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q19_1:q19_9, q19_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q19_total = sum(c_across(c(q19_1_n:q19_9_n, q19_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q19_1:q19_9, q19_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q19_1:q19_9, q19_other2), ~if_else(q19_total == 0, NA_character_ , .x)))

## q35
pp2 <- pp2 %>% mutate(q35_other2 = if_else(q35_other != "", "Ja", NA_character_), 
                    q35_other2 = factor(q35_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q35_1:q35_9), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q35_1:q35_9, q35_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q35_1:q35_9, q35_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q35_total = sum(c_across(c(q35_1_n:q35_9_n, q35_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q35_1:q35_9, q35_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q35_1:q35_9, q35_other2), ~if_else(q35_total == 0, NA_character_ , .x)))

## q44
pp2 <- pp2 %>% mutate(q44_other2 = if_else(q44_other != "", "Ja", NA_character_), 
                    q44_other2 = factor(q44_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q44_1:q44_5), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q44_1:q44_5, q44_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q44_1:q44_5, q44_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q44_total = sum(c_across(c(q44_1_n:q44_5_n, q44_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q44_1:q44_5, q44_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q44_1:q44_5, q44_other2), ~if_else(q44_total == 0, NA_character_ , .x)))

## q45
pp2 <- pp2 %>% mutate(q45_other2 = if_else(q45_other != "", "Ja", NA_character_), 
                    q45_other2 = factor(q45_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q45_1:q45_7), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q45_1:q45_7, q45_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q45_1:q45_7, q45_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q45_total = sum(c_across(c(q45_1_n:q45_7_n, q45_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q45_1:q45_7, q45_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q45_1:q45_7, q45_other2), ~if_else(q45_total == 0, NA_character_ , .x)))

## q48
pp2 <- pp2 %>% mutate(q48_other2 = if_else(q48_other != "", "Ja", NA_character_), 
                    q48_other2 = factor(q48_other2, levels = c("Nicht Gewählt", "Ja")))
pp2 <- pp2 %>% mutate(across(c(q48_1:q48_6), ~fct_drop(.x, only = "other")))
pp2 <- pp2 %>% mutate(across(c(q48_1:q48_6, q48_other2), ~fct_explicit_na(.x, "Nicht Gewählt")))
pp2 <- pp2 %>% mutate(across(c(q48_1:q48_6, q48_other2), ~as.numeric(.x)-1, .names = "{.col}_n"))
pp2 <- pp2 %>% rowwise() %>% mutate(q48_total = sum(c_across(c(q48_1_n:q48_6_n, q48_other2_n)))) %>% ungroup()
pp2 <- pp2 %>% mutate(across(c(q48_1:q48_6, q48_other2), ~as_character(.x)))
pp2 <- pp2 %>% mutate(across(c(q48_1:q48_6, q48_other2), ~if_else(q48_total == 0 | (q47 == "wurde nicht durchgeführt und ist auch nicht geplant." | q47 == "WeiÃŸ ich nicht"), NA_character_ , .x)))


## Rating Variables to numeric
pp2 <- pp2 %>% mutate(across(c(q18_1:q18_3, q41_1:q41_5, q42_1:q42_10, q43_1:q43_11, q49_1:q49_4, q50_1:q50_7), 
                           ~as_numeric(.x), .names = "{.col}_n"))

## Other gesammelt recodieren
pp2 <- pp2 %>% mutate(across(.cols = c(q20), .fns = ~fct_recode(.x, "Anderes" = "other")))

## Set Attributes
attr(pp2$q1_schulform, "label") <- "Richtung der Schulform"
attr(pp2$q1_schultyp, "label") <- "Schultyp"
attr(pp2$q14_anz_bew_kat, "label") <- "Anzahl Bewerbungen"
attr(pp2$q14_anz_bew_kat2, "label") <- "Anzahl Bewerbungen"
attr(pp2$q14_anteil_antworten, "label") <- "[Anteil der Rückmeldungen] Bewerbungen"
attr(pp2$q14_corona_absagegrund, "label") <- "[Anteil der coranbedingten Absagen bei Rückmeldungen] Bewerbungen"
attr(pp2$q31_ueberstunden, "label") <- "Arbeitsstunden pro Woche (Durchschnitt)"
attr(pp2$q59_sozialer_hintergrund, "label") <- "Höchster Bildungsabschluss eines Elternteils"
attr(pp2$q37_einkommen_kat, "label") <- "Einkommen"
attr(pp2$q37_taschengeld_kat, "label") <- "Taschengeld"
attr(pp2$q37_trinkgeld_kat, "label") <- "Trinkgeld"
attr(pp2$q37_eink_gesamt, "label") <- "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)"
attr(pp2$q37_einkommen_systematik, "label") <- "Einkommensquellen"
attr(pp2$q37_eink_gesamt, "label") <- "Einkommen gesamt (Einkommen, Taschengeld, Trinkgeld)"
attr(pp2$q39_kosten_gesamt_kat, "label") <- "Gesamtkosten"
attr(pp2$q58_notendurchschnitt, "label") <- "Notendurchschnitt (DEU, EN, Mathe)"

attr(pp2$q13_other2, "label") <- "[Sonstiges] Warum war es aus Ihrer Sicht schwierig eine Stelle zu finden?"
attr(pp2$q19_other2, "label") <- "[Sonstiges] Was war Ihnen bei der Suche nach dem Pflichtpraktikum besonders wichtig?"
attr(pp2$q35_other2, "label") <- "[Sonstiges] Von wem wurden Sie informiert?"
attr(pp2$q44_other2, "label") <- "[Sonstiges] Welche Nachweise über das Pflichtpraktikum verlangt die Schule?"
attr(pp2$q45_other2, "label") <- "[Sonstiges] In welcher Form mussten die Erfahrungen des Pflichtpraktikums festgehalten werden?"
attr(pp2$q48_other2, "label") <- "[Sonstiges] In welcher Form wurde/wird das Pflichtpraktikum nachbesprochen?"



## copy Labels from origin to new
pp2 <- copy_labels(pp2, pp2_origin)


# Variablen für Tabellenband ----
pp2_tb <- pp2 %>%
  mutate(q1_rec = fct_recode(q1,
                             # "HAK" = "Handelsakademie (HAK)",
                             # "HAS" = "Handelsschule (HAS)",
                             # "HTL" = "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
                             # "FS (techn.)" = "Gewerbliche, technische und kunstgew. Fachschulen",
                             # "HLW" = "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
                             # "FS (wirtschaft.)" = "Fachschule für wirtschaftliche Berufe",
                             # "HLT" = "Höhere Lehranstalt für Tourismus (HLT)",
                             # "FS (tourist.)" = "Hotelfachschule, Tourismusfachschule")) %>% frq(q1_rec)
                             "BHS (kaufm.)" = "Handelsakademie (HAK)",
                             "BMS (kaufm.)" = "Handelsschule (HAS)", 
                             "BHS (techn.)" = "Höhere technische und gewerbliche (einschließlich kunstgewerblicher) Lehranstalten (HTL)",
                             "BMS (techn.)" = "Gewerbliche, technische und kunstgew. Fachschulen",
                             "BHS (wirtschaft.)" = "Höhere Lehranstalten für wirtschaftliche Berufe (HLW)",
                             "BMS (wirtschaft.)" = "Fachschule für wirtschaftliche Berufe",
                             "BHS (tourist.)" = "Höhere Lehranstalt für Tourismus (HLT)",
                             "BMS (tourist.)" = "Hotelfachschule, Tourismusfachschule"),
         q1_rec1 = case_when(q1_rec %>% str_detect("kaufm") ~ "Kaufmännisch",
                             q1_rec %>% str_detect("techn") ~ "Techn.-gewerbl.",
                             q1_rec %>% str_detect("wirtsch") ~ "Wirtschaftlich",
                             q1_rec %>% str_detect("touris") ~ "Tourismus"),
         q55_rec = fct_recode(q55, NULL = "Divers"), 
         richtung_geschlecht = fct_cross(q55_rec, q1_rec1, sep = " - "),
         q28_rec = fct_recode(q28, 
                              "1-4 MA" = "1-4 MitarbeiterInnen", 
                              "5-9 MA" = "5-9 MitarbeiterInnen",
                              "10-49 MA" = "10-49 MitarbeiterInnen",
                              "50-99 MA" = "50-99 MitarbeiterInnen",
                              "100-250 MA" = "100-250 MitarbeiterInnen", 
                              "Über 250 MA" = "Über 250 MitarbeiterInnen"), 
         q57a_rec = fct_recode(q57a, 
                               "Deutsch und andere Sprache(n)" = "Überwiegend Deutsch und andere Sprache(n)", 
                               "Andere Sprache(n) und Deutsch" = "Überwiegend andere Sprache(n) und Deutsch", 
                               "Nur andere Sprache(n) als Deutsch" = "Andere Sprache(n) als Deutsch"), 
         q59_sozialer_hintergrund = fct_recode(q59_sozialer_hintergrund, 
                                               "Niedrig" = "niedrig", 
                                               "Mittel" = "mittel",
                                               "Gehoben" = "gehoben",
                                               "Hoch" = "hoch"), 
         total = "-", 
         q27_rec = fct_recode(q27, 
                              "Ja" = "Ja, das Praktikum entspricht vollständig meiner Ausbildungsrichtung", 
                              "Mit Ausbildungsrichtung verwandt" = "Ja, das Praktikum ist mit meiner Ausbildungsrichtung verwandt",
                              "Nein" = "Nein, gar nicht"), 
         q53_rec = fct_recode(q53, 
                              "Suche nach Arbeitsplatz" = "Ich werde mir einen Arbeitsplatz suchen.", 
                              "Weiterführende Ausbildung" = "Ich werde eine weiterführende Ausbildung beginnen.", 
                              "Weiß noch nicht genau" = "Weiß noch nicht genau"), 
         q24_rec = fct_recode(q24, 
                              "Regional" = "Regional, in der Nähe", 
                              "Überregional" = "Überregional, weiter weg (z.B. auch anderes Bundesland)", 
                              "Im Ausland" = "Im Ausland"),
         q1_dich = if_else(q1_rec %>% str_detect("BHS"), "BHS", "BMS") %>% as.factor(),
         regional = q24_rec %>% 
           factor(levels = c("Regional","Überregional","Im Ausland")),
         # q4 = fct_drop(q4, "Tirol"), # in PP2 gab es keinen Rücklauf aus Tirol
         across(c(q53_rec, q27_rec, q1_rec, q4, q28_rec, q57a_rec, 
                  q59_sozialer_hintergrund, q24_rec, q36, q51, q32, q12, q37a), ~ fct_drop(.x, "other"))) %>% 
  select(-matches("region"))


# cluster ----
index.var <- var.finder.label.fnc(pp2_tb,"Wie beurteilen Sie rückblickend das Pflichtp")$variable %>% 
  keep(str_detect(.,"_n$"))

### Cluster Lösung
clust_data <- pp2_tb %>% select(id, all_of(index.var)) %>% as.data.frame()
# fviz_nbclust(na.omit(clust_data %>% select(-id)), kmeans, method = "silhouette")
# fviz_nbclust(na.omit(clust_data %>% select(-id)), kmeans, method = "wss")

set.seed(1234)
# um Fälle mit missing herauszufilter
nonmis <- clust_data %>%
  filter(if_all(everything(),~ !is.na(.x))) %>%
  select(id) %>%
  mutate(cluster_qualitaet = NA)

kc.3 <- kmeans(x = na.omit(clust_data %>% select(-id)), 3)$cluster

y <- nonmis %>% 
  mutate(cluster_qualitaet = kc.3 %>% as.factor) %>% 
  # full_join(mis) %>% 
  arrange(id)

pp2_tb <- pp2_tb %>% 
  select(-matches("cluster"))

pp2_tb <- pp2_tb %>% 
  full_join(y, by = "id")

## Zuordnung Klassen zu Ordinalniveau
pp2_tb %>% 
  select(cluster_qualitaet, matches("q42.+_n")) %>% 
  pivot_longer(-cluster_qualitaet) %>%
  group_by(cluster_qualitaet,name) %>% 
  reframe(mean = mean(value,na.rm = T)) %>% 
  na.omit %>% 
  ggplot(aes(x=name, y=mean, group = cluster_qualitaet)) +
  geom_line(aes(color = cluster_qualitaet)) +
  # theme_bw() +
  theme(text = element_text(size = 14),
        legend.position="bottom",
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.line = element_line(size = .3),
        panel.background = element_blank())

pp2_tb <- pp2_tb %>%
  mutate(
    cluster_qualitaet = case_when(
      cluster_qualitaet == 1 ~ "Gruppe A (gut)",
      cluster_qualitaet == 2 ~ "Gruppe B (mittel)",
      cluster_qualitaet == 3 ~ "Gruppe C (schlecht)"
    ) %>% factor
  ) 

attr(pp2_tb$cluster_qualitaet, "label") <- "Qualität der Rahmenbedingungen mit 3 Clustern"


pp2_tb <- pp2_tb %>% 
  mutate(q37_einkommen_gesamt_kat_rec = case_when(q37_einkommen_gesamt_kat %>% str_detect("1251|1501|1751|Mehr") ~ "Mehr als 1250 Euro",
                                               .default = q37_einkommen_gesamt_kat) %>% 
           factor(levels = c("Kein Einkommen",
                             "1-250 Euro",
                             "251-500 Euro",
                             "501-750 Euro",
                             "751-1000 Euro",
                             "1001-1250 Euro",
                             "Mehr als 1250 Euro"))) 


# pp2_tb <- pp2_tb %>%
#   mutate(across(c(
#     "total",
#     "q1_dich",
#     "q1_rec",
#     "richtung_geschlecht",
#     "q4",
#     "q28_rec",
#     "q57a_rec",
#     "q59_sozialer_hintergrund",
#     "q24_rec",
#     "q53_rec",
#     "q27_rec",
#     "q36",
#     "q31_ueberstunden",
#     "q51",
#     # "cluster_qualitaet"
#   ), ~ .x %>% as.character))


attr(pp2_tb$q1_rec, "label") <- "Schulform"
attr(pp2_tb$richtung_geschlecht, "label") <- "Geschlecht - Richtung"
attr(pp2_tb$q1_schultyp, "label") <- "Schultyp"
attr(pp2_tb$q4, "label") <- "Bundesland Schule"
attr(pp2_tb$q28_rec, "label") <- "Größe Betrieb"
attr(pp2_tb$q57a_rec, "label") <- "Sprache Zuhause"
attr(pp2_tb$q59_sozialer_hintergrund, "label") <- "Familiärer Bildungshintergrund"
attr(pp2_tb$total, "label") <- "Gesamt"
attr(pp2_tb$q36, "label") <- "Unklarheiten im Dienstvertrag"
attr(pp2_tb$q27_rec, "label") <- "Entsprechung Praktikum - Ausbildungsrichtung"
attr(pp2_tb$q51, "label") <- "Betriebliches Beschäftigungsangebot nach Ausbildung"
attr(pp2_tb$q53_rec, "label") <- "Pläne nach Ausbildung"
attr(pp2_tb$q24_rec, "label") <- "Ort des Praktikums"
attr(pp2_tb$total, "label") <- "Gesamt"
attr(pp2_tb$q1_dich, "label") <- "Schultyp"
attr(pp2_tb$q31_ueberstunden, "label") <- "Arbeitsstunden pro Woche (Durchschnitt)"


## Exporting and importing pp2-data
save(pp2, pp2_dispens, pp2_origin, file = "pp2/pp2-package.RData")
saveRDS(pp2, file = "pp2/pp2.RData")
saveRDS(pp2_tb, file = "pp2/pp2_tb.RData")
