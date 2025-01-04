
test_that("`compute_bag()` `prop` agree even while `hull` and `frac` don't", {
  # data set with exactly two convex hulls
  set.seed(0)
  d <- data.frame(x = runif(12), y = runif(12))
  pc1 <- compute_bag(d, fraction = seq(0, 1, .1))
  pc2 <- compute_bag(d, fraction = seq(0, 1, .1), cut = "below")
  
  expect_true(any(pc1$hull != pc2$hull))
  expect_true(any(pc1$frac != pc2$frac))
  expect_equal(pc1$prop, pc2$prop)
})
