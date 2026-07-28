# Full pipeline: format.tbl_ord, print.tbl_ord

test_that("format default width snapshot", {
  expect_snapshot(format(ord_pca))
})

test_that("format narrow width snapshot", {
  expect_snapshot(format(ord_pca, width = 30))
})

test_that("format wide width snapshot", {
  expect_snapshot(format(ord_pca, width = 120))
})

test_that("format explicit n", {
  out <- format(ord_pca, n = 5L)
  # Should have exactly 5 data rows per factor in the output
  expect_true(any(grepl("145 more", out)))
})

test_that("format list n with NULL", {
  out <- format(ord_pca, n = list(3L, NULL))
  expect_true(is.character(out))
  expect_true(length(out) > 0L)
  # Should have 3 row data entries
  expect_true(any(grepl("147 more", out)))
})

test_that("format conference symmetric", {
  x_sym <- confer_inertia(ord_pca, 0.5)
  out <- format(x_sym)
  expect_true(any(grepl("symmetric", out)))
})

test_that("format lda", {
  out <- format(ord_lda)
  expect_true(is.character(out))
  expect_true(any(grepl("lda_ord", out)))
})

test_that("format narrow width no side-printing", {
  out <- format(ord_pca, width = 20, n = 5L)
  # At very narrow widths, annotation lines should be empty; info lines
  # starting with "#" must never appear next to coord data
  # Combined data uses " | " with spaces; compressed brackets use "|" without
  expect_false(any(grepl("^#.* \\| ", out)))
})

test_that("format pipe alignment across all lines", {
  out <- format(ord_pca, width = 80, n = 10L)
  # All body lines (not starting with #) that contain a pipe should have it at
  # the same character position
  body_lines <- out[!grepl("^#", out) & grepl("[|]", out)]
  if (length(body_lines) > 1L) {
    pipe_positions <- as.integer(regexpr("[|]", body_lines))
    expect_equal(unname(pipe_positions), rep(unname(pipe_positions[1L]), length(pipe_positions)))
  }
})

# print() S3 method returns the object invisibly -------------------------------

test_that("print returns object invisibly", {
  expect_output(ret <- withVisible(print(ord_pca)))
  expect_false(ret$visible)
  expect_identical(ret$value, ord_pca)
})

# format with n = scalar recycles to both factors -----------------------------

test_that("single values are recycled", {
  cbind(c(1, 1, 1), c(1, 2, 3), c(1, 4, 9)) %>%
    eigen() %>%
    as_tbl_ord() %>%
    negate_to_first_orthant("rows") ->
    ord
  ord_fmt <- format(ord, n = 2)
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 4L)
})

# format with list n uses per-factor values -----------------------------------

test_that("lists with NULL are accepted", {
  cbind(c(1, 1, 1), c(1, 2, 3), c(1, 4, 9)) %>%
    eigen() %>%
    as_tbl_ord() %>%
    negate_to_first_orthant("rows") ->
    ord
  # rows factor defaults, columns factor = 1
  ord_fmt <- format(ord, n = list(NULL, 1))
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 3L + 1L)
  # columns factor defaults, rows factor = 2
  ord_fmt <- format(ord, n = list(2, NULL))
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 2L + 3L)
})
