# Body content: ord_format_coord, ord_split_coord, ord_format_ann

# ord_format_coord ------------------------------------------------------------

test_that("coord basic", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  expect_true(is.list(fc))
  expect_true(is.character(fc$lines))
  expect_true(fc$n_cols_shown >= 1L)
})

test_that("coord all fit", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  expect_false(fc$has_more_cols)
  expect_equal(fc$n_cols_shown, 3L)
})

test_that("coord truncation with n", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  # header + types + 5 rows data + 5 rows data = 12 lines total
  expect_true(length(fc$lines) > 0L)
})

test_that("coord row counts match n_show", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  # fc$lines includes tibble header (names + types); data rows = min(n_show, n_dims) per factor
  data_lines <- fc$lines[-seq_len(2L)]
  actual_row_rows <- min(layout$n_show[1L], layout$n_dims[1L])
  actual_col_rows <- min(layout$n_show[2L], layout$n_dims[2L])
  expect_equal(length(data_lines), actual_row_rows + actual_col_rows)
})

test_that("coord no doubled row numbers", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  data_lines <- fc$lines[-seq_len(2L)]
  # Row numbers should be single digit for small datasets; no "11", "22" etc.
  expect_false(any(grepl("^[0-9]{2} ", data_lines)))
})

test_that("coord no leaked tibble row numbers with n=10", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 10L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  data_lines <- fc$lines[-seq_len(2L)]
  # Extract leading numbers from each data line
  leading_nums <- as.integer(sub("^\\s*(\\d+).*", "\\1", data_lines))
  n_row <- min(layout$n_show[1L], layout$n_dims[1L])
  n_col <- min(layout$n_show[2L], layout$n_dims[2L])
  # Row restart numbers: 1, 2, ..., n_row
  expect_equal(leading_nums[seq_len(n_row)], seq_len(n_row))
  # Col restart numbers: 1, 2, ..., n_col
  expect_equal(leading_nums[n_row + seq_len(n_col)], seq_len(n_col))
})

test_that("coord pipe alignment", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 10L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  # All coord lines should have the same width (padded)
  widths <- nchar(fc$lines)
  expect_equal(widths, rep(widths[1L], length(widths)))
})

test_that("coord snapshot default", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_snapshot(ord_format_coord(layout, alloc)$lines)
})

test_that("coord snapshot narrow", {
  layout <- get_ord_layout(ord_pca, width = 30)
  alloc <- ord_width_alloc(layout)
  expect_snapshot(ord_format_coord(layout, alloc)$lines)
})

test_that("coord decimal alignment with 2-digit row numbers", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 10L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  lines <- strip_style(fc$lines)
  # data lines are after the 2-line header
  data_lines <- lines[-seq_len(2L)]
  n_row <- min(layout$n_show[1L], layout$n_dims[1L])
  row_data <- data_lines[seq_len(n_row)]
  # extract the position of every "." in each data line
  dot_positions <- lapply(row_data, function(line) {
    m <- gregexpr("\\.", line)[[1]]
    if (m[1L] == -1L) integer(0) else as.integer(m)
  })
  # each data line must have the same number of decimal points
  n_dots <- vapply(dot_positions, length, integer(1))
  expect_equal(n_dots, rep(n_dots[1L], length(n_dots)))
  # and those decimal points must appear at the same column positions
  for (i in seq_along(dot_positions)[-1L]) {
    expect_equal(dot_positions[[i]], dot_positions[[1L]],
      label = paste0("decimal positions in row ", i, " match row 1")
    )
  }
  expect_snapshot(lines)
})

test_that("coord decimal alignment between row and col factors", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 10L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  lines <- strip_style(fc$lines)
  n_row <- layout$n_show[1L]
  n_col <- min(layout$n_show[2L], layout$n_dims[2L])
  row_data <- lines[seq_len(n_row) + 2L]
  col_data <- lines[seq_len(n_col) + 2L + n_row]
  all_data <- c(row_data, col_data)
  header <- lines[1L]
  n_coords <- length(gregexpr("PC[0-9]+", header)[[1]])
  # each data line must have exactly one decimal per coordinate
  for (i in seq_along(all_data)) {
    dots <- gregexpr("[.]", all_data[i])[[1]]
    dots <- dots[dots != -1L]
    expect_equal(length(dots), n_coords,
      label = paste0("line ", i, " has a decimal for each coordinate"))
  }
  # all decimal positions must be identical across every line
  ref_dots <- gregexpr("[.]", all_data[1L])[[1]]
  for (i in seq_along(all_data)[-1L]) {
    expect_equal(
      gregexpr("[.]", all_data[i])[[1]],
      ref_dots,
      label = paste0("line ", i, " decimal positions match line 1")
    )
  }
  expect_snapshot(lines)
})

# ord_split_coord -------------------------------------------------------------

test_that("ord_split_coord basic", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  expect_true(is.list(sc))
  expect_true(is.character(sc$rows))
  expect_true(is.character(sc$cols))
  # 5 row data lines + 2 header lines = 7
  expect_equal(length(sc$rows), 7L)
  # 4 col data lines + 2 header lines = 6
  expect_equal(length(sc$cols), 6L)
})

test_that("ord_split_coord header preserved", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  # Both halves share the same header lines
  expect_equal(sc$rows[1:2], sc$cols[1:2])
})

test_that("ord_split_coord small", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  # 3 row data + 2 header = 5
  expect_equal(length(sc$rows), 5L)
  # 3 col data + 2 header = 5
  expect_equal(length(sc$cols), 5L)
  expect_equal(sc$rows[1:2], sc$cols[1:2])
})

# ord_format_ann --------------------------------------------------------------

test_that("ord_format_ann basic", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_true(is.list(ann))
  expect_true(is.list(ann$rows))
  expect_true(is.list(ann$cols))
  # ord_pca has 1 row annotation and 3 col annotations
  expect_equal(ann$rows$n_total, 1L)
  expect_equal(ann$cols$n_total, 3L)
  expect_equal(ann$cols$n_cols, 3L)
})

test_that("ord_format_ann no annotations", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_equal(ann$rows$n_total, 0L)
  expect_equal(ann$cols$n_total, 0L)
  expect_equal(length(ann$rows$lines), 0L)
  expect_equal(length(ann$cols$lines), 0L)
})

test_that("ord_format_ann max_extra_cols", {
  layout <- get_ord_layout(ord_pca, width = 80, max_extra_cols = 1L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # 3 col annotations but max_extra_cols = 1
  expect_equal(ann$cols$n_cols, 1L)
  expect_equal(ann$cols$n_total, 3L)
})

test_that("ord_format_ann row numbers suppressed", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # Data lines should not start with row numbers like "1 ", "2 ", etc.
  for (line in ann$rows$lines) {
    expect_false(grepl("^[0-9]+ ", line))
  }
  for (line in ann$cols$lines) {
    expect_false(grepl("^[0-9]+ ", line))
  }
})

test_that("ord_format_ann respects n_show", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 3L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # 3 data rows + 2 header lines = 5 per factor
  expect_equal(length(ann$rows$lines), 5L)
  expect_equal(length(ann$cols$lines), 5L)
})

test_that("ord_format_ann includes header lines", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # First two lines should be col names and types, not data
  expect_true(grepl("name", ann$cols$lines[1L]))
  expect_true(grepl("<chr>", ann$cols$lines[2L]))
  expect_true(grepl(".element", ann$rows$lines[1L]))
  expect_true(grepl("<chr>", ann$rows$lines[2L]))
})

test_that("ord_format_ann value wide", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_equal(ann$rows$lines, c(
    ".element",
    "<chr>   ",
    rep("active  ", 5)
  ))
  expect_equal(ann$cols$lines, c(
    "name    center .element",
    "<chr>    <dbl> <chr>   ",
    "Sepal.~   5.84 active  ",
    "Sepal.~   3.06 active  ",
    "Petal.~   3.76 active  ",
    "Petal.~   1.20 active  "
  ))
})

test_that("ord_format_ann value narrow", {
  layout <- get_ord_layout(ord_pca, width = 30, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_equal(ann$rows$lines, c(
    ".element",
    "<chr>   ",
    rep("active  ", 5)
  ))
  expect_equal(ann$cols$lines, c(
    "name     ",
    "<chr>    ",
    "Sepal.Le~",
    "Sepal.Wi~",
    "Petal.Le~",
    "Petal.Wi~"
  ))
})

test_that("ord_format_ann value lda", {
  layout <- get_ord_layout(ord_lda, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_equal(ann$rows$lines, c(
    "name        prior counts grouping   .element",
    "<chr>       <dbl>  <int> <chr>      <chr>   ",
    "setosa      0.333     50 setosa     active  ",
    "versicolor  0.333     50 versicolor active  ",
    "virginica   0.333     50 virginica  active  ",
    "<NA>       NA         NA setosa     score   ",
    "<NA>       NA         NA setosa     score   "
  ))
  expect_equal(ann$cols$lines, c(
    "name         .element",
    "<chr>        <chr>   ",
    "Sepal.Length active  ",
    "Sepal.Width  active  ",
    "Petal.Length active  ",
    "Petal.Width  active  "
  ))
})
