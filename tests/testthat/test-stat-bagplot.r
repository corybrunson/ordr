skip_if_not_installed("ddalpha")

test_that("`stat_bagplot()` doesn't throw errors when elements are missing", {
  d0 <- data.frame(x = numeric(0), y = numeric(0))
  expect_no_error(p0 <- ggplot(d0, aes(x, y)) + stat_bagplot())
  expect_equal(nrow(layer_data(p0)), 0L)
  expect_equal(layer_grob(p0)[[1L]], zeroGrob())
})

test_that("`stat_bagplot()` encodes all plot elements unless no outliers", {
  t <- seq(0, 2/3, 1/3) * 2*pi
  d1 <- data.frame(x = cos(t), y = sin(t))
  expect_no_error(p1 <- ggplot(d1, aes(x, y)) + stat_bagplot())
  expect_setequal(unique(layer_data(p1)$component), c("median", "bag", "fence"))
  
  d2 <- data.frame(x = 2*cos(t+1/3*pi), y = 2*sin(t+1/3*pi))
  d3 <- rbind(d1, d2)
  expect_no_error(p3 <- ggplot(d3, aes(x, y)) + stat_bagplot(coef = 1))
  expect_setequal(unique(layer_data(p3)$component),
                  c("median", "bag", "fence", "outliers"))
})
