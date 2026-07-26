test_that("format returns character", {
  out <- format(ord_pca)
  expect_true(is.character(out))
  expect_true(length(out) > 0L)
})

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

test_that("format list n", {
  out <- format(ord_pca, n = list(3L, NULL))
  expect_true(is.character(out))
  expect_true(length(out) > 0L)
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
  expect_false(any(grepl("^#.*\\|", out)))
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
