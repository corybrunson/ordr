fit_prcomp <- prcomp(iris[, -5], scale = TRUE)

test_that("'prcomp' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_prcomp)), ncol(get_cols(fit_prcomp)))
  expect_equal(ncol(get_rows(fit_prcomp)), length(recover_inertia(fit_prcomp)))
})

test_that("'prcomp' has specified distribution of inertia", {
  expect_type(recover_conference(fit_prcomp), "double")
  expect_vector(recover_conference(fit_prcomp), size = 2L)
})

test_that("'prcomp' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_prcomp)),
               ".element" %in% names(recover_aug_cols(fit_prcomp)))
})

test_that("`as_tbl_ord()` coerces 'prcomp' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_prcomp)))
})
