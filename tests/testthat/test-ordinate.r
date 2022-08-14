set.seed(1)
rdata <- data.frame(
  a = rnorm(n = 5L, mean = 5),
  b = rpois(n = 5L, lambda = 3),
  c = runif(n = 5L)
)

test_that("`ordinate()` can handle appropriate models", {
  expect_error(ordinate(rdata, lra, everything()), regexp = NA)
  expect_error(ordinate(rdata, prcomp, everything()), regexp = NA)
  expect_error(ordinate(rdata, princomp, everything()), regexp = NA)
})
