context("logistic SVD and PCA, classes 'lsvd', 'lpca', & 'clpca'")

data(finches, package = "cooccur")
fit_lsvd <- logisticSVD_ord(t(finches))
fit_lpca <- logisticPCA_ord(t(finches))
fit_clpca <- convexLogisticPCA_ord(t(finches[-13L, , drop = FALSE]))

test_that("`as_tbl_ord()` coerces 'lsvd', 'lpca', & 'clpca' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lsvd)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lpca)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_clpca)))
})
