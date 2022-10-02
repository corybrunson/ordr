fit_svd <- svd_ord(USPersonalExpenditure)

test_that("'svd_ord' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_svd)), ncol(get_cols(fit_svd)))
  expect_equal(ncol(get_rows(fit_svd)), length(recover_inertia(fit_svd)))
})

test_that("'svd_ord' has specified distribution of inertia", {
  expect_type(recover_conference(fit_svd), "double")
  expect_vector(recover_conference(fit_svd), size = 2L)
})

test_that("'svd_ord' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_svd)),
               ".element" %in% names(recover_aug_cols(fit_svd)))
})

test_that("`as_tbl_ord()` coerces 'svd_ord' objects", {
  expect_equal(class(fit_svd), "svd_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_svd)))
})
