fit_eigen <- eigen_ord(cbind(c(1,-1), c(-1,1)))

test_that("'eigen_ord' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_eigen)), ncol(get_cols(fit_eigen)))
  expect_equal(ncol(get_rows(fit_eigen)), length(recover_inertia(fit_eigen)))
})

test_that("'eigen_ord' has specified distribution of inertia", {
  expect_type(recover_conference(fit_eigen), "double")
  expect_vector(recover_conference(fit_eigen), size = 2L)
})

test_that("'eigen_ord' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_eigen)),
               ".element" %in% names(recover_aug_cols(fit_eigen)))
})

test_that("`as_tbl_ord()` coerces 'eigen_ord' objects", {
  expect_true("eigen_ord" %in% class(fit_eigen))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_eigen)))
})
