# body content

# `ord_format_coord()` ---------------------------------------------------------

test_that("coordinates all fit", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  expect_false(fc$has_more_cols)
  expect_equal(fc$n_cols_shown, 3L)
})

test_that("coordinate row counts match `n_show`", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  data_lines <- fc$lines[-seq_len(2L)]
  actual_row_rows <- min(layout$n_show[1L], layout$n_dims[1L])
  actual_col_rows <- min(layout$n_show[2L], layout$n_dims[2L])
  expect_equal(length(data_lines), actual_row_rows + actual_col_rows)
})

test_that("coordinates with no doubled row numbers", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  data_lines <- fc$lines[-seq_len(2L)]
  # row numbers should be single-digit for small datasets
  expect_true(any(grepl("^[0-9] ", strip_style(data_lines))))
  expect_false(any(grepl("^[0-9]{2} ", strip_style(data_lines))))
})

test_that("coordinate lines align (for separator alignment)", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 10L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  # all coordinate lines should have the same (padded) width
  widths <- nchar(fc$lines)
  expect_equal(widths, rep(widths[1L], length(widths)))
})

test_that("coordinate snapshot (default)", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_snapshot(ord_format_coord(layout, alloc)$lines)
})

test_that("coordinate snapshot (narrow)", {
  layout <- get_ord_layout(ord_pca, width = 30)
  alloc <- ord_width_alloc(layout)
  expect_snapshot(ord_format_coord(layout, alloc)$lines)
})

test_that("coordinate decimal points align between factors", {
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

# `ord_split_coord()` ----------------------------------------------------------

test_that("`ord_split_coord()` splits correctly and preserves headers", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  # 5 row data lines + 2 header lines = 7
  expect_equal(length(sc$rows), 7L)
  # 4 col data lines + 2 header lines = 6
  expect_equal(length(sc$cols), 6L)
  # names lines match; sub-headers are factor-specific
  expect_equal(sc$rows[1], sc$cols[1])
  expect_match(sc$rows[2], "\\[[0-9\\.]+\\]")
  expect_match(sc$cols[2], "\\[[0-9\\.]+\\]")

  # small object: no conference, so both fall back to types line
  layout_small <- get_ord_layout(ord_small, width = 80)
  alloc_small <- ord_width_alloc(layout_small)
  fc_small <- ord_format_coord(layout_small, alloc_small)
  sc_small <- ord_split_coord(fc_small, layout_small)
  expect_equal(length(sc_small$rows), 5L)
  expect_equal(length(sc_small$cols), 5L)
  expect_equal(sc_small$rows[1:2], sc_small$cols[1:2])
})

# `ord_format_ann()` -----------------------------------------------------------

test_that("`ord_format_ann()` basic", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # `ord_pca` has 1 row annotation and 3 column annotations
  expect_equal(ann$rows$n_total, 1L)
  expect_equal(ann$cols$n_total, 3L)
  expect_equal(ann$cols$n_cols, 3L)
})

test_that("`ord_format_ann()` without annotations", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_equal(ann$rows$n_total, 0L)
  expect_equal(ann$cols$n_total, 0L)
  expect_equal(length(ann$rows$lines), 0L)
  expect_equal(length(ann$cols$lines), 0L)
})

test_that("width-driven hiding", {
  # at narrow width some annotations do not fit and pillar hides them;
  # `n_cols` reflects the width-fitted count
  layout <- get_ord_layout(ord_pca, width = 30)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  expect_lt(ann$cols$n_cols, ann$cols$n_total)
  expect_gt(ann$cols$n_cols, 0L)
})

test_that("row numbers are suppressed", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # annotation lines should not start with row numbers
  for (line in ann$rows$lines) {
    expect_false(grepl("^[0-9]+ ", line))
  }
  for (line in ann$cols$lines) {
    expect_false(grepl("^[0-9]+ ", line))
  }
})

test_that("include header lines", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  ann <- ord_format_ann(layout, alloc)
  # first two lines should be variable names and types
  expect_true(grepl("name", ann$cols$lines[1L]))
  expect_true(grepl("<chr>", ann$cols$lines[2L]))
  expect_true(grepl(".element", ann$rows$lines[1L]))
  expect_true(grepl("<chr>", ann$rows$lines[2L]))
})
