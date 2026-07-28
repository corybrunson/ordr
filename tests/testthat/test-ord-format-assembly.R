# Assembly: ord_header, ord_footer, ord_assemble

# ord_header ------------------------------------------------------------------

test_that("ord_header responds to width and conference", {
  layout <- get_ord_layout(ord_pca, width = 80)
  hdr <- ord_header(layout)
  expect_true(is.character(hdr))
  expect_true(any(grepl("prcomp", hdr)))
  expect_true(any(grepl("PC1", hdr)))
  expect_true(any(grepl("Rows", hdr)))
  expect_true(any(grepl("Columns", hdr)))

  layout30 <- get_ord_layout(ord_pca, width = 30)
  hdr30 <- ord_header(layout30)
  expect_false(any(grepl("PC1", hdr30)))

  layout25 <- get_ord_layout(ord_pca, width = 25)
  hdr25 <- ord_header(layout25)
  expect_true(any(grepl("A tbl_ord", hdr25)))
})

test_that("ord_header displays conference type", {
  x_sym <- confer_inertia(ord_pca, 0.5)
  layout_sym <- get_ord_layout(x_sym, width = 80)
  hdr_sym <- ord_header(layout_sym)
  expect_true(any(grepl("symmetric", hdr_sym)))

  x_std <- confer_inertia(ord_pca, 0)
  layout_std <- get_ord_layout(x_std, width = 80)
  hdr_std <- ord_header(layout_std)
  expect_true(any(grepl("standard", hdr_std)))
})

test_that("ord_header no annotations", {
  layout <- get_ord_layout(ord_small, width = 80)
  hdr <- ord_header(layout)
  expect_true(any(grepl("| 0 ]", hdr)))
})

test_that("ord_header snapshot full", {
  layout <- get_ord_layout(ord_pca, width = 80)
  expect_snapshot(ord_header(layout))
})

test_that("ord_header snapshot narrow", {
  layout <- get_ord_layout(ord_pca, width = 35)
  expect_snapshot(ord_header(layout))
})

test_that("ord_header snapshot very narrow", {
  layout <- get_ord_layout(ord_pca, width = 20)
  expect_snapshot(ord_header(layout))
})

test_that("ord_header snapshot lda", {
  layout <- get_ord_layout(ord_lda, width = 80)
  expect_snapshot(ord_header(layout))
})

# ord_footer ------------------------------------------------------------------

test_that("footer basic with truncation", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  ftr <- ord_footer(layout)
  # common footer (e.g. "more rows") is currently disabled
  ftr_all <- do.call(c, ftr)
  expect_false(any(grepl("145 more rows", ftr_all)))
})

test_that("footer no truncation when all rows shown", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L)
  ftr <- ord_footer(layout)
  expect_false(any(grepl("more rows", do.call(c, ftr))))
})

test_that("footer max_extra_cols", {
  # ord_pca cols have 3 annotation cols; set max_extra_cols = 1
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  ftr <- ord_footer(layout)
  expect_true(any(grepl("more variable", ftr$cols_var)))
})

test_that("footer lists hidden variable names", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  ftr <- ord_footer(layout)
  # Should list the hidden annotation variable names
  expect_true(any(grepl("center", ftr$cols_var)))
  expect_true(any(grepl(".element", ftr$cols_var)))
})

test_that("footer max_footer_lines", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L, max_footer_lines = 1L)
  ftr <- ord_footer(layout)
  # max_footer_lines currently only limits common footer, which is disabled
  expect_true(length(ftr$common %||% character()) <= 1L)
})

test_that("footer no duplication", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  ftr <- ord_footer(layout)
  # The "more rows" count line should appear at most once (currently disabled)
  rows_lines <- grep("more rows", ftr$rows_var, value = TRUE)
  cols_lines <- grep("more rows", ftr$cols_var, value = TRUE)
  expect_true(length(c(rows_lines, cols_lines)) <= 1L)
})

test_that("footer styled grey", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  ftr <- ord_footer(layout)
  # All lines should be styled grey when crayon is available
  ftr_all <- do.call(c, ftr)
  if (crayon::has_color()) {
    expect_true(all(grepl("\033", ftr_all, fixed = TRUE)))
  }
})

test_that("footer snapshot", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  expect_snapshot(strip_style(c(
    ord_footer(layout)$rows_var,
    ord_footer(layout)$cols_var
  )))
})

test_that("footer snapshot all rows", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L)
  expect_snapshot(strip_style(c(
    ord_footer(layout)$rows_var,
    ord_footer(layout)$cols_var
  )))
})

test_that("footer snapshot with max_extra_cols", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  expect_snapshot(strip_style(c(
    ord_footer(layout)$rows_var,
    ord_footer(layout)$cols_var
  )))
})

test_that("footer snapshot narrow", {
  layout <- get_ord_layout(ord_pca, width = 30, n = 5L)
  expect_snapshot(strip_style(c(
    ord_footer(layout)$rows_var,
    ord_footer(layout)$cols_var
  )))
})

test_that("footer snapshot very narrow", {
  layout <- get_ord_layout(ord_pca, width = 20, n = 5L)
  expect_snapshot(strip_style(c(
    ord_footer(layout)$rows_var,
    ord_footer(layout)$cols_var
  )))
})

# ord_assemble ----------------------------------------------------------------

test_that("print snapshot stripped", {
  expect_snapshot(strip_style(format(ord_pca)))
})

test_that("ord_assemble concatenates header, body, and footer in order", {
  header <- c("# header line 1", "# header line 2")
  body <- list(rows = c("row line 1", "row line 2"), cols = c("col line 1"))
  footer <- list(rows_var = character(), cols_var = character())
  out <- ord_assemble(header, body, footer)
  expect_true(is.character(out))
  expect_equal(out[1:2], header)
  expect_equal(out[3], "row line 1")
  expect_equal(out[4], "row line 2")
  expect_equal(out[5], "col line 1")

  header3 <- c("# h1", "# h2", "# h3")
  body3 <- list(rows = paste("r", 1:5), cols = paste("c", 1:4))
  footer0 <- list(rows_var = character(), cols_var = character())
  out3 <- ord_assemble(header3, body3, footer0)
  expect_equal(length(out3), 3L + 5L + 4L)

  header1 <- c("# h1")
  body1 <- list(rows = "r1", cols = "c1")
  footer1 <- list(rows_var = character(), cols_var = character())
  out1 <- ord_assemble(header1, body1, footer1)
  expect_equal(length(out1), 3L)
  expect_equal(out1[3], "c1")

  header_empty_body <- c("# h1")
  body_empty <- list(rows = character(), cols = character())
  footer_empty <- list(rows_var = character(), cols_var = character())
  out_empty <- ord_assemble(header_empty_body, body_empty, footer_empty)
  expect_equal(length(out_empty), 1L)
  expect_equal(out_empty, c("# h1"))
})
