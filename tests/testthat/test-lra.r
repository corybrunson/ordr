context("log-ratio analysis, class 'lra'")

test_that("`lra()` varies by parameters", {
  expect_error(lra(USArrests[, -3]), regexp = NA)
  expect_error(lra(USArrests[, -3], compositional = TRUE), regexp = NA)
  expect_error(lra(USArrests[, -3], weighted = FALSE), regexp = NA)
  expect_error(lra(USArrests[, -3], compositional = TRUE, weighted = FALSE),
               regexp = NA)
})

fit_lra <- lra(USArrests[, -3])

test_that("`as_tbl_ord()` coerces 'lra' objects", {
  expect_equal(class(fit_lra), "lra")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lra)))
})

test_that("'lra' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_lra)),
               ".element" %in% names(augmentation_cols(fit_lra)))
})
