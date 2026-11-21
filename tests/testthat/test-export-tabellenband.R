test_that("tb_format_value formats percentages and means", {
  expect_equal(tb_format_value(c(0, 12.4, NA), "perc"), c("-", "12", "-"))
  expect_equal(tb_format_value(c(1.666, NA), "mean"), c("1,67", "-"))
})

test_that("tb_prepare drops Total and (Missing) rows", {
  df <- data.frame(
    variable = c("Gesamt", "SCHULTYP", "-", "-", "-"),
    variable_label = c("Gesamt", "Schultyp", "-", "-", "-"),
    category = c("Gesamt", "BMS", "BHS", "(Missing)", "Total"),
    stat = rep("Valid Percent (weighted)", 5),
    Ja = c(50, 40, 60, NA, 50),
    Nein = c(50, 60, 40, NA, 50),
    Total = c(100, 100, 100, NA, 100),
    `n_valid.(unweighted)` = c(100, 40, 60, 0, 100),
    `n_missing.(unweighted)` = c(0, 0, 0, 10, 0),
    check.names = FALSE
  )
  tb <- tb_prepare(df, sheet = "Q01")

  expect_equal(nrow(tb$display), 3)
  expect_equal(tb$display$row, c("-", "BMS", "BHS"))
  expect_equal(tb$display$grp, c("Gesamt", "Schultyp", ""))
  expect_true("Total" %in% tb$item_cols)
  expect_equal(tb$total_row, 1)
})

test_that("tb_split creates balanced blocks", {
  tb <- list(item_cols = paste0("i", 1:15))
  blocks <- tb_split(tb, max_items = 11)
  expect_length(blocks, 2)
  expect_length(blocks[[1]]$item_cols, 8)
  expect_length(blocks[[2]]$item_cols, 7)
})

test_that("tb_bar_png writes a PNG", {
  path <- tempfile(fileext = ".png")
  tb_bar_png(50, 100, path)
  expect_true(file.exists(path))
  expect_gt(file.info(path)$size, 0)
})
