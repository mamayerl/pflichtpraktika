#' Read tabellenband sheets from an Excel workbook
#'
#' Reads every sheet (or a selected set) of the workbook produced by the
#' tabellenband scripts into a named list of data frames.
#'
#' @param path Path to the `.xlsx` file.
#' @param sheets Optional character vector of sheet names. Defaults to all
#'   sheets in the workbook.
#'
#' @return A named list of data frames.
#' @export
read_tb_sheets <- function(path, sheets = NULL) {
  if (is.null(sheets)) {
    sheets <- openxlsx::getSheetNames(path)
  }
  stats::setNames(
    lapply(sheets, function(s) {
      openxlsx::read.xlsx(path, sheet = s, check.names = FALSE)
    }),
    sheets
  )
}

#' Clean a sanitised column label for display
#'
#' The tabellenband functions store variable labels as column names, where
#' spaces and punctuation have been replaced by dots. This reverses the most
#' common patterns and collapses the remaining dots to spaces.
#'
#' @param x Character vector of column names.
#' @param label_map Optional named character vector with manual overrides.
#'
#' @return A character vector of display labels.
#' @keywords internal
tb_clean_label <- function(x, label_map = NULL) {
  out <- x
  override <- rep(NA_character_, length(out))
  if (!is.null(label_map)) {
    hit <- match(x, names(label_map))
    override[!is.na(hit)] <- unname(label_map[hit[!is.na(hit)]])
  }
  out <- gsub("z\\.\\.B\\.\\.", "z. B. ", out)
  out <- gsub("d\\.\\.h\\.\\.", "d. h. ", out)
  out <- gsub("u\\.\\.a\\.\\.", "u. a. ", out)
  out <- gsub("bzw\\.\\.", "bzw. ", out)
  out <- gsub("\\.", " ", out)
  out <- gsub("[[:space:]]+", " ", out)
  out <- trimws(out)
  out[out == "Total"] <- "Gesamt"
  out[!is.na(override)] <- override[!is.na(override)]
  out
}

#' Default display labels for the break variables
#' @keywords internal
tb_break_label <- function(x) {
  map <- c(
    SCHULTYP = "Schultyp",
    SCHULFORM = "Schulform",
    GESCHLECHT_FELD = "Geschlecht - Richtung",
    AUSBILDUNGSFELD = "Ausbildungsfeld",
    BUNDESLAND = "Bundesland Schule",
    BETRIEBSGROESSE = "Größe Betrieb",
    SPRACHE_DE = "Sprache Zuhause",
    BILDUNGSHINTERGRUND = "Familiärer Bildungshintergrund",
    PRAKTIKUMSORT = "Ort des Praktikums",
    PLAENE = "Pläne nach Ausbildung",
    ENTSPRECHUNG = "Entsprechung Praktikum - Ausbildungsrichtung",
    UNKLARHEITEN = "Unklarheiten im Dienstvertrag",
    ARBEITSSTUNDEN = "Arbeitsstunden pro Woche (Durchschnitt)",
    BESCHAEFTIGUNGSANGEBOT = "Betriebliches Beschäftigungsangebot nach Ausbildung",
    CLUSTER_QUALITAET_3 = "Qualität der Rahmenbedingungen mit 3 Clustern",
    BERUFSBEREICH = "In welchem Berufsbereich haben Sie Ihr Praktikum absolviert?"
  )
  out <- unname(map[x])
  out[is.na(out)] <- x[is.na(out)]
  out
}

#' First non-empty label in a metadata column
#' @keywords internal
tb_first_label <- function(x) {
  x <- as.character(x)
  x <- x[!is.na(x) & x != "-" & x != ""]
  if (length(x) == 0) "" else x[1]
}

#' Format a statistic for display
#' @keywords internal
tb_format_value <- function(x, type) {
  if (type == "mean") {
    out <- ifelse(
      is.na(x),
      "-",
      formatC(x, format = "f", digits = 2, decimal.mark = ",")
    )
    return(out)
  }
  out <- ifelse(
    is.na(x),
    "-",
    ifelse(
      x == 0,
      "-",
      formatC(round(x), format = "d", big.mark = ".", decimal.mark = ",")
    )
  )
  out
}

#' Prepare one Excel sheet for the Word table
#'
#' Turns a sheet from [read_tb_sheets()] into a display matrix with one row per
#' break category and one column per item/answer category, plus a case-number
#' column. The overall row (`variable == "Gesamt"`) is kept first, `Total`
#' rows and the `n_missing` column are dropped.
#'
#' @param df A data frame read from the tabellenband workbook.
#' @param sheet Sheet name, used as the question code.
#' @param label_map Optional named character vector for header overrides.
#' @param drop_missing Drop `(Missing)` rows (default `TRUE`).
#' @param keep_total_col Keep the `Total` column (default `TRUE`).
#'
#' @return A list with the prepared display data and metadata.
#' @export
tb_prepare <- function(
  df,
  sheet = "",
  label_map = NULL,
  drop_missing = TRUE,
  keep_total_col = TRUE
) {
  nm <- names(df)
  stat_col <- which(nm == "stat")[1]
  if (is.na(stat_col)) {
    stop("Sheet '", sheet, "' has no 'stat' column.", call. = FALSE)
  }

  data_cols <- nm[-seq_len(stat_col)]
  n_col <- grep("^n_valid|^n_min", data_cols, value = TRUE)[1]
  item_cols <- data_cols[!grepl("^n_", data_cols)]
  if (!keep_total_col) {
    item_cols <- item_cols[item_cols != "Total"]
  }

  stat_vals <- unique(stats::na.omit(as.character(df$stat)))
  type <- if (any(grepl("mean", stat_vals, ignore.case = TRUE))) {
    "mean"
  } else {
    "perc"
  }

  title_col <- intersect(c("item_variable_name", "byvariable_label"), nm)[1]
  title <- if (!is.na(title_col)) {
    trimws(tb_first_label(df[[title_col]]))
  } else {
    sheet
  }

  cat <- as.character(df$category)
  variable <- if ("variable" %in% nm) {
    as.character(df$variable)
  } else {
    rep("-", nrow(df))
  }
  variable_filled <- variable
  if (length(variable_filled) > 1) {
    for (i in 2:length(variable_filled)) {
      if (is.na(variable_filled[i]) || variable_filled[i] == "-") {
        variable_filled[i] <- variable_filled[i - 1]
      }
    }
  }
  grp <- if ("variable_label" %in% nm) {
    as.character(df$variable_label)
  } else {
    rep("-", nrow(df))
  }
  grp[is.na(grp) | grp == "-"] <- NA_character_
  if (length(grp) > 1) {
    for (i in 2:length(grp)) {
      if (is.na(grp[i])) {
        grp[i] <- grp[i - 1]
      }
    }
  }
  grp[is.na(grp)] <- tb_break_label(variable_filled[is.na(grp)])

  is_total_row <- !is.na(cat) &
    cat == "Gesamt" &
    (variable == "Gesamt" | is.na(variable))
  keep <- !is.na(cat) & cat != "" & cat != "Total"
  if (drop_missing) {
    keep <- keep & cat != "(Missing)"
  }
  # keep the overall row even when its category would be filtered
  keep <- keep | is_total_row
  keep[is.na(keep)] <- FALSE

  display <- data.frame(
    grp = ifelse(is_total_row, "Gesamt", grp),
    row = ifelse(is_total_row, "-", cat),
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  for (col in item_cols) {
    display[[col]] <- tb_format_value(df[[col]], type)
  }
  n_values <- if (!is.na(n_col)) {
    ifelse(
      is.na(df[[n_col]]),
      "-",
      formatC(df[[n_col]], format = "d", big.mark = ".", decimal.mark = ",")
    )
  } else {
    rep("-", nrow(df))
  }
  display[["n"]] <- n_values
  display <- display[keep, , drop = FALSE]

  grp_vec <- display$grp
  is_start <- c(TRUE, grp_vec[-1] != grp_vec[-length(grp_vec)])
  group_rows <- which(is_start & grp_vec != "Gesamt")
  group_end <- c(group_rows[-1] - 1, nrow(display))
  display$grp[!is_start] <- ""

  header_labels <- c(
    grp = "Fragestellung",
    row = "Kategorie",
    stats::setNames(tb_clean_label(item_cols, label_map), item_cols),
    n = "n (gültig)"
  )

  list(
    sheet = sheet,
    title = title,
    subtitle = if (type == "mean") {
      "Angaben als Mittelwert (1 = sehr gut)"
    } else {
      "Angaben in Prozent"
    },
    type = type,
    display = display,
    item_cols = item_cols,
    header_labels = header_labels,
    total_row = which(display$grp == "Gesamt")[1],
    group_rows = group_rows,
    group_end = group_end
  )
}

#' Split a prepared table into blocks of at most `max_items` item columns
#' @keywords internal
tb_split <- function(tb, max_items = 11) {
  k <- length(tb$item_cols)
  if (k <= max_items) {
    return(list(tb))
  }
  n_blocks <- ceiling(k / max_items)
  sizes <- rep(ceiling(k / n_blocks), n_blocks)
  sizes[length(sizes)] <- k - sum(sizes[-length(sizes)])
  out <- vector("list", n_blocks)
  start <- 1
  for (b in seq_len(n_blocks)) {
    idx <- start:(start + sizes[b] - 1)
    start <- start + sizes[b]
    block <- tb
    block$item_cols <- tb$item_cols[idx]
    block$display <- tb$display[,
      c("grp", "row", tb$item_cols[idx], "n"),
      drop = FALSE
    ]
    block$header_labels <- tb$header_labels[c(
      "grp",
      "row",
      tb$item_cols[idx],
      "n"
    )]
    block$title <- if (b == 1) tb$title else paste0(tb$title, " (Fortsetzung)")
    out[[b]] <- block
  }
  out
}

#' Split a prepared table at the start of each grouping variable
#'
#' The overall row (`Gesamt`) stays with the first grouping variable, so a
#' segment always starts with a group label (or with the overall row).
#'
#' @param tb A prepared table from [tb_prepare()].
#'
#' @return A list of prepared tables, one per grouping variable.
#' @keywords internal
tb_split_groups <- function(tb) {
  grp <- tb$display$grp
  starts <- which(!is.na(grp) & grp != "")
  # row 1 is the overall row: it belongs to the first grouping variable
  if (!is.na(tb$total_row) && length(starts) > 1) {
    starts <- starts[-2]
  }
  if (length(starts) < 2) {
    return(list(tb))
  }
  ends <- c(starts[-1] - 1, nrow(tb$display))
  lapply(seq_along(starts), function(k) {
    rows <- starts[k]:ends[k]
    block <- tb
    block$display <- tb$display[rows, , drop = FALSE]
    rownames(block$display) <- NULL
    block$total_row <- if (!is.na(tb$total_row) && tb$total_row %in% rows) {
      match(tb$total_row, rows)
    } else {
      NA_integer_
    }
    block$group_rows <- which(block$display$grp != "")
    block$group_end <- integer(0)
    block
  })
}

#' Estimated height of a table in points
#'
#' The model is calibrated on rendered output: a row is about 18.5 points high
#' regardless of the font size, every additional wrapped line adds about 6
#' points. Line counts are estimated generously, so the estimate errs towards
#' too tall rather than too short.
#'
#' @param display The `display` data frame of a prepared table.
#' @param font_size Body font size in points.
#' @param label_w Width of the two label columns in cm.
#'
#' @return Height of the table body in points.
#' @keywords internal
tb_table_height <- function(display, font_size, label_w = c(3.0, 3.2)) {
  chars_per_line <- (label_w * 28.3465) / (0.48 * font_size)
  lines <- pmax(
    ceiling(nchar(display$grp) / chars_per_line[1]),
    ceiling(nchar(display$row) / chars_per_line[2]),
    1
  )
  18.5 + (lines - 1) * 6
}

#' Paginate a prepared table before grouping variables
#'
#' Groups are kept whole: a group that does not fit on the current page starts
#' a new page. A single group taller than a page is left as one block and may
#' overflow.
#'
#' @param tb A prepared table from [tb_prepare()] / [tb_split()].
#' @param usable_pt Usable page height in points.
#' @param font_size Body font size in points.
#' @param header_pt Height of the repeated table head in points.
#' @param fill Share of the page height that may be used, compensates for the
#'   uncertainty of the height estimate.
#'
#' @return A list of prepared tables, one per page.
#' @keywords internal
tb_paginate <- function(tb, usable_pt, font_size, header_pt, fill = 1) {
  segments <- tb_split_groups(tb)
  if (length(segments) < 2) {
    return(segments)
  }
  size <- vapply(
    segments,
    function(s) sum(tb_table_height(s$display, font_size)),
    numeric(1)
  )
  capacity <- (usable_pt - header_pt) * fill

  pages <- list()
  current <- integer(0)
  used <- 0
  for (k in seq_along(segments)) {
    if (length(current) > 0 && used + size[k] > capacity) {
      pages[[length(pages) + 1]] <- current
      current <- integer(0)
      used <- 0
    }
    current <- c(current, k)
    used <- used + size[k]
  }
  pages[[length(pages) + 1]] <- current

  lapply(pages, function(idx) {
    page <- tb
    page$display <- do.call(rbind, lapply(segments[idx], function(s) s$display))
    rownames(page$display) <- NULL
    page$total_row <- NA_integer_
    offset <- 0
    for (s in segments[idx]) {
      if (!is.na(s$total_row)) {
        page$total_row <- offset + s$total_row
      }
      offset <- offset + nrow(s$display)
    }
    page$group_rows <- which(page$display$grp != "")
    page$group_end <- integer(0)
    page
  })
}

#' Create a transparent bar PNG
#'
#' @param value Numeric value.
#' @param max_value Upper bound of the scale.
#' @param path Output file.
#' @param color Bar colour.
#' @param width,height Pixel dimensions.
#'
#' @return The file path, invisibly.
#' @export
tb_bar_png <- function(
  value,
  max_value,
  path,
  color = "#EC7D30",
  width = 280,
  height = 80
) {
  grDevices::png(
    path,
    width = width,
    height = height,
    bg = "transparent",
    type = "cairo"
  )
  on.exit(grDevices::dev.off(), add = TRUE)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, max_value), ylim = c(0, 1))
  if (!is.na(value) && value > 0) {
    graphics::rect(0, 0, value, 1, col = color, border = NA)
  }
  invisible(path)
}

#' Cached bar image for a value
#' @keywords internal
tb_bar_file <- function(value, max_value, cache_dir) {
  key <- sprintf("bar_%s_%s.png", round(value * 100), round(max_value * 100))
  path <- file.path(cache_dir, key)
  if (!file.exists(path)) {
    tb_bar_png(value, max_value, path)
  }
  path
}

#' Build a flextable for one prepared block
#'
#' @param tb A prepared block from [tb_prepare()] / [tb_split()].
#' @param font_size Body font size in points.
#' @param header_size Header font size in points.
#' @param header_height Header row height in cm.
#' @param keep_groups Keep the rows of a grouping variable together, so a page
#'   break falls before a group and never inside it.
#'
#' @return A `flextable` object.
#' @export
tb_flextable <- function(
  tb,
  font_size = 8,
  header_size = 8,
  header_height = 3.6,
  keep_groups = TRUE
) {
  df <- tb$display
  ncol_all <- ncol(df)
  n_items <- length(tb$item_cols)

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(ft, values = as.list(tb$header_labels))
  ft <- flextable::font(ft, fontname = "Calibri", part = "all")
  ft <- flextable::fontsize(ft, size = font_size, part = "body")
  ft <- flextable::fontsize(ft, size = header_size, part = "header")
  ft <- flextable::padding(ft, padding = 0.02, part = "all")
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::align(ft, j = 1:2, align = "left", part = "all")
  ft <- flextable::align(ft, j = 3:ncol_all, align = "right", part = "body")
  ft <- flextable::align(ft, j = 3:ncol_all, align = "center", part = "header")

  # title and subtitle header rows
  ft <- flextable::add_header_row(
    ft,
    values = c("", "", tb$subtitle, ""),
    colwidths = c(1, 1, ncol_all - 3, 1)
  )
  ft <- flextable::add_header_row(
    ft,
    values = c(paste0("Fragecode: ", sub("_.*$", "", tb$sheet)), tb$title),
    colwidths = c(1, ncol_all - 1)
  )

  # rotate only the bottom header row
  ft <- flextable::rotate(
    ft,
    i = 3,
    j = 3:ncol_all,
    rotation = "btlr",
    part = "header"
  )
  ft <- flextable::height(
    ft,
    i = 3,
    height = header_height,
    part = "header",
    unit = "cm"
  )
  ft <- flextable::hrule(ft, i = 3, rule = "exact", part = "header")

  # widths
  label_w <- c(3.0, 3.2)
  n_w <- 1.0
  data_w <- 17 - sum(label_w) - n_w
  item_w <- min(2.0, data_w / n_items)
  widths <- c(label_w[1], label_w[2], rep(item_w, n_items), n_w)
  ft <- flextable::width(ft, j = seq_along(widths), width = widths, unit = "cm")

  # borders
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(width = 0.5, color = "#999999")
  thin_v <- officer::fp_border(width = 0.5, color = "#CCCCCC")
  thick <- officer::fp_border(width = 1.2, color = "#000000")
  ft <- flextable::border_outer(ft, border = thin, part = "all")
  ft <- flextable::border_inner_h(ft, border = thin, part = "body")
  ft <- flextable::border_inner_v(ft, border = thin_v, part = "all")
  ft <- flextable::hline_bottom(ft, border = thick, part = "header")
  if (length(tb$group_rows) > 0) {
    for (r in tb$group_rows) {
      ft <- flextable::border(
        ft,
        i = r,
        border.top = thick,
        part = "body"
      )
    }
  }

  # shading and bold group labels
  if (!is.na(tb$total_row)) {
    ft <- flextable::bg(
      ft,
      i = tb$total_row,
      j = 1:2,
      bg = "#D3D3D3",
      part = "body"
    )
    ft <- flextable::bg(
      ft,
      i = tb$total_row,
      j = 3:ncol_all,
      bg = "#CCCCCC",
      part = "body"
    )
  }
  if (length(tb$group_rows) > 0) {
    ft <- flextable::bold(ft, i = tb$group_rows, j = 1, part = "body")
  }
  # Gruppen bleiben zusammen: alle Zeilen einer Gruppe mit der Folgezeile
  # verketten, damit ein Seitenumbruch vor der Gruppe und nicht in ihr landet
  if (keep_groups && length(tb$group_rows) > 0) {
    keep_i <- unlist(
      Map(
        function(s, e) if (e > s) seq.int(s, e - 1L) else integer(0),
        tb$group_rows,
        tb$group_end
      ),
      use.names = FALSE
    )
    if (!is.na(tb$total_row)) {
      keep_i <- union(keep_i, tb$total_row)
    }
  } else {
    keep_i <- tb$group_rows
  }
  if (length(keep_i) > 0) {
    ft <- flextable::keep_with_next(ft, i = keep_i, part = "body")
  }
  ft <- flextable::bold(ft, part = "header")

  # bars in the overall row (all data columns except a constant Total column)
  if (!is.na(tb$total_row)) {
    cache_dir <- file.path(tempdir(), "tb_bars")
    dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE)
    all_values <- suppressWarnings(as.numeric(sub(
      ",",
      ".",
      unlist(df[, tb$item_cols, drop = FALSE])
    )))
    max_value <- if (tb$type == "mean") {
      max(all_values, na.rm = TRUE)
    } else {
      100
    }
    for (j in seq_along(tb$item_cols)) {
      col <- tb$item_cols[j]
      value <- suppressWarnings(as.numeric(
        sub(",", ".", df[tb$total_row, col])
      ))
      text <- df[tb$total_row, col]
      text_w <- max(nchar(text)) * font_size * 0.018
      bar_w <- max(0.14, item_w - text_w - 0.12)
      bar_h <- max(0.12, font_size * 0.022)
      if (col == "Total") {
        next
      }
      if (!is.na(value)) {
        img <- tb_bar_file(value, max_value, cache_dir)
        ft <- flextable::compose(
          ft,
          i = tb$total_row,
          j = j + 2,
          value = flextable::as_paragraph(
            flextable::as_image(
              src = img,
              width = bar_w,
              height = bar_h,
              unit = "cm"
            ),
            flextable::as_chunk(
              paste0(" ", text),
              props = officer::fp_text(
                font.family = "Calibri",
                font.size = font_size
              )
            )
          )
        )
      }
    }
  }

  ft <- flextable::set_table_properties(ft, layout = "fixed", align = "center")
  ft
}

#' Write a Word tabellenband from an Excel workbook
#'
#' @param xlsx Path to the Excel workbook (one sheet per table).
#' @param docx Output `.docx` path.
#' @param template Path to the Word template that provides page setup and
#'   header/footer.
#' @param sheets Optional subset of sheets.
#' @param label_map Optional named character vector for header overrides.
#' @param drop_missing Drop `(Missing)` rows.
#' @param keep_total_col Keep the `Total` column as `Gesamt`.
#' @param max_items Maximum number of item columns per table block. Wider
#'   sheets are split into balanced blocks.
#' @param break_before_group Keep grouping variables whole: if a table does not
#'   fit on one page, the page break is placed before a grouping variable
#'   instead of inside a group. With `FALSE` Word breaks pages on its own.
#'
#' @return The output path, invisibly.
#' @export
write_tabellenband_docx <- function(
  xlsx,
  docx,
  template,
  sheets = NULL,
  label_map = NULL,
  drop_missing = TRUE,
  keep_total_col = TRUE,
  max_items = 11,
  break_before_group = TRUE
) {
  default_map <- c(
    "Schulische.Vorgaben.passten.nicht.zu.betrieblichen.Anforderungen.(z..B..bei.der.Dauer.der.angebotenen.Praktikumsplätze)" = "Schulische Vorgaben passten nicht zu betrieblichen Anforderungen",
    "Persönliche.Gründe.(z.B..Krankheit,.Pflege,.Betreuungspflichten)" = "Persönliche Gründe (Krankheit, Pflege, Betreuung)",
    "Keine/zu.wenig.Unterstützung.seitens.der.Schule.erhalten" = "Keine/zu wenig Unterstützung durch die Schule",
    "Keine/zu.wenig.Unterstützung.seitens.der.Familie.erhalten" = "Keine/zu wenig Unterstützung durch die Familie"
  )
  if (!is.null(label_map)) {
    default_map[names(label_map)] <- label_map
  }
  label_map <- default_map

  sheets_data <- read_tb_sheets(xlsx, sheets)
  doc <- officer::read_docx(template)
  dims <- officer::docx_dim(doc)
  # nutzbare Seitenhöhe minus etwas Reserve (in Punkten)
  usable_pt <- (unname(dims$page["height"]) -
    unname(dims$margins["top"]) -
    unname(dims$margins["bottom"])) *
    72 -
    14
  first_table <- TRUE

  for (i in seq_along(sheets_data)) {
    sheet <- names(sheets_data)[i]
    tb <- tb_prepare(
      sheets_data[[i]],
      sheet = sheet,
      label_map = label_map,
      drop_missing = drop_missing,
      keep_total_col = keep_total_col
    )
    blocks <- tb_split(tb, max_items = max_items)

    for (b in seq_along(blocks)) {
      block <- blocks[[b]]
      n_items <- length(block$item_cols)
      font_size <- if (n_items > 10) {
        6.5
      } else if (n_items > 7) {
        7
      } else {
        8
      }
      header_size <- if (n_items > 10) {
        6.5
      } else if (n_items > 7) {
        7
      } else {
        8
      }
      max_chars <- max(nchar(unlist(block$header_labels[block$item_cols])))
      header_height <- min(7, max(2, max_chars * 0.07 + 0.8))

      # wiederholter Tabellenkopf: Titel, Untertitel, gedrehte Kopfzeile
      header_pt <- header_height * 28.3465 + 2 * 18.5

      pages <- if (break_before_group) {
        # 5 % Reserve, weil die Hoehen geschaetzt und nicht gemessen sind
        tb_paginate(
          block,
          usable_pt = usable_pt,
          font_size = font_size,
          header_pt = header_pt,
          fill = 0.95
        )
      } else {
        list(block)
      }

      for (page in pages) {
        ft <- tb_flextable(
          page,
          font_size = font_size,
          header_size = header_size,
          header_height = header_height,
          keep_groups = break_before_group
        )

        # Jede Tabelle beginnt auf einer neuen Seite
        if (!first_table) {
          doc <- officer::body_add_break(doc, pos = "after")
        }
        first_table <- FALSE
        doc <- flextable::body_add_flextable(doc, ft)
      }
    }
  }

  print(doc, target = docx)
  invisible(docx)
}
