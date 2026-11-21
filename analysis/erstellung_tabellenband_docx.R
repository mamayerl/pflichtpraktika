#################################################
## Excel-Tabellenband in ein Word-Dokument ueberfuehren
#################################################

devtools::load_all()

write_tabellenband_docx(
  xlsx = "PP4_proto_tabellenband.xlsx",
  docx = "PP4_tabellenband.docx",
  template = "template/Tabellenband_template.docx"
)
