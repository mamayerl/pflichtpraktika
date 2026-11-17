library(tidyverse)
library(rstudioapi)
library(flextable)
library(sjmisc)
library(sjlabelled)
library(janitor)
library(officer)
library(openxlsx)

# Tabellenband

## Erzeugen

ft.list <- readRDS("ft_list_3.Rdata")
# ft.fragen <- readRDS(file = "ft.fragen.textmarken.RData")


# Word mit Tabellen erstellen
doc <- read_docx("Tabellenband_template.docx")
# doc <- read_docx("Tabellenband_template-heading1.docx")
for (i in 1:23) {
  # Textmarke einfügen
  # doc <- body_add(doc, value = fpar(ft.fragen$Frage[i]), style = "heading 1")
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < 23) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband1.docx")

doc <- read_docx("Tabellenband_template.docx")
for (i in 24:47) {
  # Textmarke einfügen
  # doc <- body_add(doc, value = fpar(ft.fragen$Frage[i]), style = "heading 1")
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < 47) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband2.docx")


doc <- read_docx("Tabellenband_template.docx")
for (i in 48:length(ft.list)) {
  # Textmarke einfügen
  # doc <- body_add(doc, value = fpar(ft.fragen$Frage[i]), style = "heading 1")
  doc <- body_add_flextable(doc, value = ft.list[[i]])
  # Add a page break after each flextable (except the last one)
  if (i < length(ft.list)) {
    doc <- body_add_break(doc, pos = "after")
  }
}
print(doc, target = "Tabellenband3.docx")



doc <- read_docx("../bericht/Tabellenband/tabellenband_pp_einleitung.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband1.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband2.docx")
doc <- body_add_break(doc, pos = "after")
doc <- body_add_docx(doc, "Tabellenband3.docx")
saveRDS(doc, "ft_doc.RData")

# print(doc, target = "Tabellenband_vorlaeufig.docx")
print(doc, target = "../bericht/Tabellenband/Tabellenband_vorlaeufig.docx")
system("open ../bericht/Tabellenband/Tabellenband_vorlaeufig.docx")


### H?ndische ?nderungen in Fu?noten oder Kopfzeilen
# F4 Teil 1
# F4 Teil 2
# 
# E1 Teil 1
# 
# D2 Teil 1: Fu?note Ende und bei Zahl
# D2 Teil 2: Fu?note Ende und bei Zahl
# 
# B3: Fu?note Ende und bei Zahl

# A9: Seitenumbruch

# library(doconv)
# docx2pdf(input = "Tabellenband_vorlaeufig.docx",
#          output = "Tabellenband_vorlaeufig.pdf")

