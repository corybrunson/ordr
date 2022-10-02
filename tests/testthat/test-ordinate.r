set.seed(1)
rdata <- data.frame(
  a = rnorm(n = 5L, mean = 5),
  b = rpois(n = 5L, lambda = 3),
  c = runif(n = 5L)
)

test_that("`ordinate()` can handle `lra()` & `princomp()` with a data frame", {
  expect_error(ordinate(rdata, lra), regexp = NA)
  expect_error(ordinate(rdata, ~ princomp(.)), regexp = NA)
})

rdist <- stats::dist(rdata)

test_that("`ordinate()` can handle `cmdscale_ord()` with a 'dist' object", {
  expect_error(ordinate(rdist, "cmdscale_ord"), regexp = NA)
})
