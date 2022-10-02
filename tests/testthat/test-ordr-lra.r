test_that("`lra()` varies by parameters", {
  expect_error(lra(USArrests[, -3]), regexp = NA)
  expect_error(lra(USArrests[, -3], compositional = TRUE), regexp = NA)
  expect_error(lra(USArrests[, -3], weighted = FALSE), regexp = NA)
  expect_error(lra(USArrests[, -3], compositional = TRUE, weighted = FALSE),
               regexp = NA)
})

fit_lra <- lra(USArrests[, -3])

test_that("lra accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_lra)), ncol(get_cols(fit_lra)))
  expect_equal(ncol(get_rows(fit_lra)), length(recover_inertia(fit_lra)))
})

test_that("lra has specified distribution of inertia", {
  expect_type(recover_conference(fit_lra), "double")
  expect_vector(recover_conference(fit_lra), size = 2L)
})

test_that("'lra' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_lra)),
               ".element" %in% names(recover_aug_cols(fit_lra)))
})

test_that("`as_tbl_ord()` coerces 'lra' objects", {
  expect_equal(class(fit_lra), "lra")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lra)))
})
