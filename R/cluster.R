#' @import data.table

# Skalenwerte aus as_factor-Faktoren ziehen ("1 - sehr gut" -> 1)
scale_num <- function(x) {
  suppressWarnings(as.numeric(sub("^\\s*(\\d+).*$", "\\1", as.character(x))))
}

# Zuordnung der Clusternummern zu inhaltlichen Gruppen
cluster_levels <- list(
  "2" = c("Gruppe A (gut)", "Gruppe B (schlecht)"),
  "3" = c("Gruppe A (gut)", "Gruppe B (mittel)", "Gruppe C (schlecht)"),
  "4" = c(
    "Gruppe A (sehr gut)",
    "Gruppe B (gut)",
    "Gruppe C (schlecht)",
    "Gruppe D (sehr schlecht)"
  )
)

# K-Means-Cluster der Qualitäts-Items als Breakvariablen
rec_cluster <- function(
  data,
  items = paste0("Q45_", 1:10),
  k = 4:2,
  seed = 1234
) {
  x <- as.data.frame(lapply(data[, ..items], scale_num))
  ok <- stats::complete.cases(x)

  set.seed(seed)
  for (n in k) {
    levels_n <- cluster_levels[[as.character(n)]]
    km <- stats::kmeans(x[ok, , drop = FALSE], centers = n)

    # Cluster nach mittlerer Note ordnen (1 = sehr gut)
    score <- rowMeans(x[ok, , drop = FALSE])
    ord <- order(tapply(score, km$cluster, mean))

    vals <- rep(NA_character_, nrow(data))
    vals[ok] <- levels_n[match(km$cluster, ord)]

    var <- paste0("CLUSTER_QUALITAET_", n)
    data[, (var) := factor(vals, levels = levels_n)]
    setattr(
      data[[var]],
      "label",
      paste0("Qualität der Rahmenbedingungen mit ", n, " Clustern")
    )
  }
  return(data)
}
