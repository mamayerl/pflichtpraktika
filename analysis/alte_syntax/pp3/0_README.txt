README.txt

Die reguläre Datenaufbereitung für PP3 geschieht in pp3_auswertung_v1.Rmd. Dafür muss aus gewichtung.R das darin berechnete Gewicht angefügt werden.

Die Tabellen für den regulären Tabellenband werden mit tb-1-tabellen-flextables.R berechnet und als  Liste mit Flextables gespeichert. In tb-2-docs-erstellen.R werden Word-Dokumente mit den Flextables erstellt, die dann für den finalen Tabellenband zusammengefasst werden. Die ursprüngliche Idee, mehrere Word-Dokumente zu erstellen lag darin, Zwischenstände abzuspeichern und evtl. die Rechenleistung zu reduzieren. Das war allerdings nur eine Hypothese.

Den zeitlichen Vergleichstabellenband erstellen die Skripte tb-vergleich-pp2.R und tb-vergleich-pp3.R. Die Dokumente werden in Vergleich-Zeit/word-erzeugen.R zusammengeführt.

Individuelle Rückmeldungen an die Bildungsdirektionen bzw. Schulen werden mittels auswertung-BL.R und auswertung-schulen.R erstellt.

Die Skripte können über rstudioapi::jobRunScript() ausgeführt werden.