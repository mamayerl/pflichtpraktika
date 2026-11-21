#################################################
## Excel-Tabellenband in ein Word-Dokument ueberfuehren
#################################################

devtools::load_all()

## To do: Verhalten bei break_before_group = TRUE
# funktioniert leider noch nicht so gut.
write_tabellenband_docx(
  xlsx = "PP4_proto_tabellenband.xlsx",
  docx = "PP4_tabellenband.docx",
  template = "template/Tabellenband_template.docx",
  break_before_group = FALSE
)
