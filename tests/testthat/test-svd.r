context("singular value decomposition, class 'svd_ord'")

fit_svd <- svd_ord(USPersonalExpenditure)

test_that("`as_tbl_ord()` coerces 'svd_ord' objects", {
  expect_equal(class(fit_svd), "svd_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_svd)))
})

test_that("'svd_ord' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_svd)),
               ".element" %in% names(augmentation_cols(fit_svd)))
})
