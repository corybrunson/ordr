context("tidier for eigendecomposition, class 'eigen'")

fit_eigen <- eigen(cbind(c(1,-1), c(-1,1)))

test_that("`tidy()` method formats 'eigen' objects", {
  expect_equal(tidy(fit_eigen, matrix = "lambda")$inertia, fit_eigen$values)
  expect_equal(tidy(fit_eigen)$value, as.vector(fit_eigen$vectors))
})
