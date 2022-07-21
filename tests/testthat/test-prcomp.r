context("principal components analysis, class 'prcomp'")

fit_prcomp <- prcomp(iris[, -5], scale = TRUE)

test_that("`as_tbl_ord()` coerces 'prcomp' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_prcomp)))
})

test_that("'prcomp' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_prcomp)),
               ".element" %in% names(augmentation_cols(fit_prcomp)))
})
