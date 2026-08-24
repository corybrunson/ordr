# assembly

# `ord_header()` ---------------------------------------------------------------

test_that("respond to width and conference", {
  layout <- get_ord_layout(ord_pca, width = 80)
  hdr <- ord_header(layout)
  # expected text elements at full width tier
  expect_true(any(grepl("prcomp", hdr)))
  expect_true(any(grepl("PC1", hdr)))
  expect_true(any(grepl("Rows", hdr)))
  expect_true(any(grepl("Columns", hdr)))
  # expected text elements at low width tier
  layout30 <- get_ord_layout(ord_pca, width = 30)
  hdr30 <- ord_header(layout30)
  expect_false(any(grepl("PC1", hdr30)))
  # expected text elements at minimal width tier
  layout25 <- get_ord_layout(ord_pca, width = 25)
  hdr25 <- ord_header(layout25)
  expect_false(any(grepl("^A ", hdr25)))
  expect_true(any(grepl("^# tbl_ord:", hdr25)))
  expect_false(any(grepl("%", hdr25)))
})

test_that("display conference type", {
  x_sym <- confer_inertia(ord_pca, 0.5)
  layout_sym <- get_ord_layout(x_sym, width = 80)
  hdr_sym <- ord_header(layout_sym)
  expect_true(any(grepl("symmetric", hdr_sym)))

  x_std <- confer_inertia(ord_pca, 0)
  layout_std <- get_ord_layout(x_std, width = 80)
  hdr_std <- ord_header(layout_std)
  expect_true(any(grepl("standard", hdr_std)))
  expect_true(any(grepl("principal", hdr_std)))
})

test_that("headers (full)", {
  layout <- get_ord_layout(ord_pca, width = 80)
  expect_snapshot(ord_header(layout))
})

test_that("headers (narrow)", {
  layout <- get_ord_layout(ord_pca, width = 35)
  expect_snapshot(ord_header(layout))
})

test_that("headers (very narrow)", {
  layout <- get_ord_layout(ord_pca, width = 20)
  expect_snapshot(ord_header(layout))
})

test_that("headers (lda)", {
  layout <- get_ord_layout(ord_lda, width = 80)
  expect_snapshot(ord_header(layout))
})

# `ord_footer()` ---------------------------------------------------------------

test_that("no truncation when all rows shown", {
  ftr <- footer_of(ord_pca, width = 80, n = 150L)
  expect_false(any(grepl("more rows", do.call(c, ftr))))
})

test_that("note only columns hidden by width", {
  # wide output prints all annotations regardless of `max_extra_cols`,
  # which now only caps the footer listing
  ftr <- footer_of(ord_pca, width = 80, n = 150L, max_extra_cols = 1L)
  expect_equal(length(do.call(c, ftr)), 0L)

  # narrow output hides column annotations by width and so notes them;
  # the single row annotation still fits, so no rows note
  ftr30 <- footer_of(ord_pca, width = 30, n = 150L)
  expect_true(any(grepl("more variable", unwrap_note(ftr30$cols_var))))
  expect_equal(length(ftr30$rows_var), 0L)
})

test_that("list hidden variable names", {
  ftr <- footer_of(ord_pca, width = 30, n = 150L)
  # hidden by width: center and .element (only name is printed);
  # default max_extra_cols lists all hidden names without an ellipsis
  note <- unwrap_note(ftr$cols_var)
  expect_true(any(grepl("more variable", note)))
  expect_true(any(grepl("center", note, fixed = TRUE)))
  expect_true(any(grepl(".element", note, fixed = TRUE)))
  expect_false(any(grepl("...", note, fixed = TRUE)))
})

test_that("cap listing at `max_extra_cols` with ellipsis", {
  ftr <- footer_of(ord_pca, width = 30, n = 150L, max_extra_cols = 1L)
  # true total reported, listing capped to the first hidden name
  note <- unwrap_note(ftr$cols_var)
  expect_true(any(grepl("2 more variables", note)))
  expect_true(any(grepl("...", note, fixed = TRUE)))
  expect_true(any(grepl("center", note, fixed = TRUE)))
  expect_false(any(grepl(".element", note, fixed = TRUE)))
})

test_that("cap lines at `max_footer_lines`", {
  # strict budget: one line per component, marked with ellipsis
  ftr1 <- footer_of(ord_lda, width = 40, n = 5L, max_footer_lines = 1L)
  n_lines <- lapply(ftr1, function(v) lengths(strsplit(v, "\n", fixed = TRUE)))
  expect_equal(unlist(n_lines), c(1L, 1L), ignore_attr = TRUE)
  expect_true(all(grepl("\u2026", unlist(ftr1), fixed = TRUE)))

  # generous budget: match default footer without ellipsis
  ftr_def <- footer_of(ord_lda, width = 40, n = 5L)
  ftr_big <- footer_of(ord_lda, width = 40, n = 5L, max_footer_lines = 10L)
  expect_equal(ftr_def, ftr_big)
  expect_false(any(grepl("\u2026", unlist(ftr_def), fixed = TRUE)))

  # exact fit: columns note (3 lines) is unmarked while rows note is capped
  ftr3 <- footer_of(ord_lda, width = 40, n = 5L, max_footer_lines = 3L)
  expect_true(grepl("\u2026", strip_style(ftr3$rows_var), fixed = TRUE))
  expect_false(grepl("\u2026", strip_style(ftr3$cols_var), fixed = TRUE))
})

test_that("grey styling", {
  ftr <- footer_of(ord_lda, width = 40, n = 5L)
  ftr_all <- do.call(c, ftr)
  if (crayon::has_color()) {
    expect_true(all(grepl("\033", ftr_all, fixed = TRUE)))
  }
})

test_that("footer snapshot", {
  ftr <- footer_of(ord_pca, width = 80, n = 5L)
  expect_snapshot(strip_style(c(ftr$rows_var, ftr$cols_var)))
})

test_that("footer snapshot all rows", {
  ftr <- footer_of(ord_pca, width = 80, n = 150L)
  expect_snapshot(strip_style(c(ftr$rows_var, ftr$cols_var)))
})

test_that("footer snapshot with max_extra_cols", {
  ftr <- footer_of(ord_pca, width = 30, n = 150L, max_extra_cols = 1L)
  expect_snapshot(strip_style(c(ftr$rows_var, ftr$cols_var)))
})

test_that("footer snapshot narrow", {
  ftr <- footer_of(ord_pca, width = 30, n = 5L)
  expect_snapshot(strip_style(c(ftr$rows_var, ftr$cols_var)))
})

test_that("footer snapshot very narrow", {
  ftr <- footer_of(ord_pca, width = 20, n = 5L)
  expect_snapshot(strip_style(c(ftr$rows_var, ftr$cols_var)))
})

# `ord_assemble()` -------------------------------------------------------------

test_that("print snapshot (stripped)", {
  expect_snapshot(strip_style(format(ord_pca)))
})
