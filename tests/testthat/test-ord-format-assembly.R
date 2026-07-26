# Assembly: ord_header, ord_footer, ord_assemble

# ord_header ------------------------------------------------------------------

test_that("ord_header basic", {
  layout <- get_ord_layout(ord_pca, width = 80)
  hdr <- ord_header(layout)
  expect_true(is.character(hdr))
  expect_true(any(grepl("prcomp", hdr)))
  expect_true(any(grepl("PC1", hdr)))
  expect_true(any(grepl("Rows", hdr)))
  expect_true(any(grepl("Columns", hdr)))
})

test_that("ord_header narrow omits coord names", {
  layout <- get_ord_layout(ord_pca, width = 30)
  hdr <- ord_header(layout)
  # At width 30, coord names line should be omitted
  expect_false(any(grepl("PC1", hdr)))
})

test_that("ord_header very narrow is minimal", {
  layout <- get_ord_layout(ord_pca, width = 25)
  hdr <- ord_header(layout)
  expect_true(any(grepl("A tbl_ord", hdr)))
})

test_that("ord_header conference symmetric", {
  x_sym <- confer_inertia(ord_pca, 0.5)
  layout <- get_ord_layout(x_sym, width = 80)
  hdr <- ord_header(layout)
  expect_true(any(grepl("symmetric", hdr)))
})

test_that("ord_header conference standard", {
  x_std <- confer_inertia(ord_pca, 0)
  layout <- get_ord_layout(x_std, width = 80)
  hdr <- ord_header(layout)
  expect_true(any(grepl("standard", hdr)))
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
  expect_true(any(grepl("145 more rows", ftr)))
})

test_that("footer no truncation when all rows shown", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L)
  ftr <- ord_footer(layout)
  expect_false(any(grepl("more rows", ftr)))
})

test_that("footer max_extra_cols", {
  # ord_pca cols have 3 annotation cols; set max_extra_cols = 1
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  ftr <- ord_footer(layout)
  expect_true(any(grepl("more variable", ftr)))
})

test_that("footer lists hidden variable names", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  ftr <- ord_footer(layout)
  # Should list the hidden annotation variable names
  expect_true(any(grepl("center", ftr)))
  expect_true(any(grepl(".element", ftr)))
})

test_that("footer max_footer_lines", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L, max_footer_lines = 1L)
  ftr <- ord_footer(layout)
  expect_true(length(ftr) <= 1L)
})

test_that("footer no duplication", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  ftr <- ord_footer(layout)
  ftr_plain <- strip_style(ftr)
  # The "more rows" count line should appear at most once
  row_lines <- grep("more rows", ftr_plain, value = TRUE)
  expect_true(length(row_lines) <= 1L)
})

test_that("footer styled grey", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  ftr <- ord_footer(layout)
  # All lines should be styled grey when crayon is available
  # (strip_style removes ANSI; if all lines were styled, original has ANSI)
  if (crayon::has_color()) {
    expect_true(all(grepl("\033", ftr, fixed = TRUE)))
  }
})

test_that("footer snapshot", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  expect_snapshot(strip_style(ord_footer(layout)))
})

test_that("footer snapshot all rows", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L)
  expect_snapshot(strip_style(ord_footer(layout)))
})

test_that("footer snapshot with max_extra_cols", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  expect_snapshot(strip_style(ord_footer(layout)))
})

test_that("footer snapshot narrow", {
  layout <- get_ord_layout(ord_pca, width = 30, n = 5L)
  expect_snapshot(strip_style(ord_footer(layout)))
})

test_that("footer snapshot very narrow", {
  layout <- get_ord_layout(ord_pca, width = 20, n = 5L)
  expect_snapshot(strip_style(ord_footer(layout)))
})

# ord_assemble ----------------------------------------------------------------

test_that("ord_assemble basic", {
  header <- c("# header line 1", "# header line 2")
  body <- list(rows = c("row line 1", "row line 2"), cols = c("col line 1"))
  footer <- c("# footer line 1")
  out <- ord_assemble(header, body, footer)
  expect_true(is.character(out))
  expect_equal(out[1:2], header)
  expect_equal(out[3], "row line 1")
  expect_equal(out[4], "row line 2")
  expect_equal(out[5], "col line 1")
  expect_equal(out[6], "# footer line 1")
})

test_that("ord_assemble line count", {
  header <- c("# h1", "# h2", "# h3")
  body <- list(rows = paste("r", 1:5), cols = paste("c", 1:4))
  footer <- character()
  out <- ord_assemble(header, body, footer)
  expect_equal(length(out), 3L + 5L + 4L + 0L)
})

test_that("ord_assemble no footer", {
  header <- c("# h1")
  body <- list(rows = "r1", cols = "c1")
  footer <- character()
  out <- ord_assemble(header, body, footer)
  expect_equal(length(out), 3L)
  expect_equal(out[3], "c1")
})

test_that("ord_assemble empty body", {
  header <- c("# h1")
  body <- list(rows = character(), cols = character())
  footer <- c("# f1")
  out <- ord_assemble(header, body, footer)
  expect_equal(length(out), 2L)
  expect_equal(out, c("# h1", "# f1"))
})
