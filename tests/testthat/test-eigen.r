context("eigendecomposition, class 'eigen_ord'")

fit_eigen <- eigen_ord(cbind(c(1,-1), c(-1,1)))

test_that("`as_tbl_ord()` coerces 'eigen_ord' objects", {
  expect_equal(class(fit_eigen), "eigen_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_eigen)))
})
