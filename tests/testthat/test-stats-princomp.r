fit_princomp <- princomp(iris[, -5], cor = FALSE)

test_that("'princomp' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_princomp)), ncol(get_cols(fit_princomp)))
  expect_equal(ncol(get_rows(fit_princomp)),
               length(recover_inertia(fit_princomp)))
})

test_that("'princomp' has specified distribution of inertia", {
  expect_type(recover_conference(fit_princomp), "double")
  expect_vector(recover_conference(fit_princomp), size = 2L)
})

test_that("'princomp' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_princomp)),
               ".element" %in% names(recover_aug_cols(fit_princomp)))
})

test_that("`as_tbl_ord()` coerces 'princomp' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_princomp)))
})
