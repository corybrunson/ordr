fit_factanal <- factanal(swiss, factors = 3L, scores = "regression")

test_that("'factanal' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_factanal)), ncol(get_cols(fit_factanal)))
  expect_equal(ncol(get_rows(fit_factanal)),
               length(recover_inertia(fit_factanal)))
})

test_that("'factanal' has specified distribution of inertia", {
  expect_type(recover_conference(fit_factanal), "double")
  expect_vector(recover_conference(fit_factanal), size = 2L)
})

test_that("'factanal' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_factanal)),
               ".element" %in% names(recover_aug_cols(fit_factanal)))
})

test_that("`as_tbl_ord()` coerces 'factanal' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_factanal)))
})
