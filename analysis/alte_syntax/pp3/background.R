# Laden ----
library(tidyverse)
library(flextable)
library(officer)
library(doconv)

ft.list <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/ft.list.RData")
schulnamen.df <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulnamen.df.RData")  
schulen.gefiltert <- readRDS(file = "../rueckmeldungen/rueckmeldungen_schulen/schulen_gefiltert.RData")


bold_face <- shortcuts$fp_bold()
doc.list <- list()
for (k in 31:length(ft.list)) {
  # for (k in 1:1) {
  
  read_docx(path = "Tabellenband_rueckmeldungen_template.docx") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "", style = "Normal") %>%
    body_add_par(value = "Erfahrungen mit Pflichtpraktika in den berufsbildenden Schulen", style = "Title") %>%
    body_add_par(value = "-", style = "Subtitle") %>%
    body_add_par(value = "Schulspezifische Rückmeldungen für den Schulstandort", style = "Subtitle") %>%
    # body_add_par(value = "-", style = "Subtitle") %>%
    body_add_par(value = names(schulen.gefiltert)[k], style = "Subtitle") %>%
    body_add_par(value = "Sehr geehrte Direktionen,", style = "Fließtext") %>%
    body_add_par(value = "das Österreichische Institut für Berufsbildungsforschung (öibf) bedankt sich sehr herzlich für die Mitwirkung Ihrer Schule an dieser Studie, die von der Arbeiterkammer Wien und dem Bundesministerium für Bildung, Wissenschaft und Forschung beauftragt wurde. Die Ergebnisse liegen jetzt in einer umfangreichen Berichtsform vor, die wir Ihnen hiermit gerne zur Verfügung stellen.", style = "Fließtext") %>%
    body_add_par(value = "Als speziellen Dank für Ihre Teilnahme dürfen wir Ihnen auch eine schulspezifische Auswertung übermitteln, in der Sie die Ergebnisse Ihrer Schule mit jenen der Gesamtergebnisse vergleichen können. Um die Anonymität nach den Schulstandorten zu gewährleisten, haben die Auftraggeber keine schulspezifische Auswertung erhalten. Die Ergebnisse dienen ausschließlich zu Ihrer internen Verwendung.", style = "Fließtext") %>%
    body_add_par(value = "", style = "Fließtext") %>%
    body_add_par(value = "", style = "Fließtext") %>%
    body_add_fpar(value = fpar(ftext("Hinweise zur Interpretation:", prop = bold_face)), style = "Fließtext") %>%
    body_add_par(value = 'In den Vergleichsgruppen finden sich Schulen für die groben Ausbildungsrichtungen "kaufmännisch", "technisch-gewerblich", "wirtschaftsberufliche", "touristisch", „pädagogisch“ und „sozialberuflich“. Für genauere Informationen zur Stichprobe dürfen wir auf den Endbericht verweisen, der auf unserer Website http://www.oeibf.at frei abrufbar ist.', style = "List Bullet") %>%
    body_add_par(value = "Bei den schulspezifischen Auswertungen können die jeweiligen Stichproben sehr klein sein. Daraus ergeben sich erhebliche statistische Schwankungsbreiten. Die Ergebnisse können aus methodischer Sicht daher lediglich als Orientierungshilfe gesehen werden, um eventuelle Tendenzen abzuleiten.", style = "List Bullet") %>%
    body_add_par(value = 'Einige Fragen wurden als Filterfragen konzipiert. Dies spiegelt sich in einer hohen Anzahl von fehlenden Werten wider. Diese wurden mit "-" gekennzeichnet. Ein Prozentwert von "0 %" verweist auf einen gerundeten Wert kleiner als 0,5 %.', style = "List Bullet") %>%  
    body_add_par(value = "", style = "Fließtext") %>%
    body_add_par(value = "", style = "Fließtext") %>%
    body_add_fpar(value = fpar("Herzlichen Dank für Ihre Unterstützung, auch im Namen des Projektteams!", 
                               run_linebreak(), 
                               ftext("Dr. Norbert Lachmayr"),
                               run_linebreak(), 
                               ftext("öibf - Margaretenstraße 166/2. Stock, 1050 Wien; ZVR-Nr.: 718743404; www.oeibf.at")),
                  style = "Fließtext_li") %>%
    body_add_break(pos = "after") %>% 
    body_add_flextable(value = ft.list[[k]]) %>%
    print(target = paste0("../rueckmeldungen/rueckmeldungen_schulen/word/", schulnamen.df$Anschrift[k], ".docx"))
  
  docx2pdf(input = paste0("../rueckmeldungen/rueckmeldungen_schulen/word/", schulnamen.df$Anschrift[k], ".docx"),
           output = paste0("../rueckmeldungen/rueckmeldungen_schulen/pdfs/", schulnamen.df$Anschrift[k], ".pdf"))
  
  }
