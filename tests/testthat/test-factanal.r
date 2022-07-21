context("factor analysis, class 'factanal'")

fit_factanal <- factanal(swiss, factors = 3L, scores = "regression")

test_that("`as_tbl_ord()` coerces 'factanal' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_factanal)))
})

test_that("'factanal' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_factanal)),
               ".element" %in% names(augmentation_cols(fit_factanal)))
})
