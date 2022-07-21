context("principal components analysis, class 'princomp'")

fit_princomp <- princomp(iris[, -5], cor = FALSE)

test_that("`as_tbl_ord()` coerces 'princomp' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_princomp)))
})

test_that("'princomp' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_princomp)),
               ".element" %in% names(augmentation_cols(fit_princomp)))
})
