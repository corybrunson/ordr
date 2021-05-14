context("singular value decomposition, class 'svd_ord'")

fit_svd <- svd_ord(USPersonalExpenditure)

test_that("`as_tbl_ord()` coerces 'svd_ord' objects", {
  expect_equal(class(fit_svd), "svd_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_svd)))
})
