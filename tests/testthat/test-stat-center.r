# toy elongated data set
t <- seq(6)/6 * 2*pi
m <- cbind(cos(t), sin(t))
d <- as.data.frame( m %*% matrix(c(1,1.25,1.75,1), nrow = 2) )
names(d) <- c("x", "y")
p <- ggplot(d, aes(x, y))
p1 <- p + stat_center(fun.data = mean_se)

test_that("`stat_center()` correctly handles anonymous functions", {
  p2 <- p + stat_center(fun.data = \(z) {
    zest <- mean(z)
    zse <- sqrt(var(z) / length(z))
    data.frame(y = zest, ymin = zest - zse, ymax = zest + zse)
  })
  expect_equal(layer_data(p1), layer_data(p2))
})

test_that("`fun.ord` generalizes `fun.data`", {
  p3 <- p + stat_center(fun.ord = \(z) {
    zest <- apply(z, 2L, mean)
    zse <- sqrt( apply(z, 2L, var) / nrow(z) )
    zmin <- zest - zse; zmax <- zest + zse
    names(zmin) <- paste0(names(zmin), "min")
    names(zmax) <- paste0(names(zmax), "max")
    c(zest, zmin, zmax)
  })
  expect_equal(layer_data(p1)[names(layer_data(p3))], layer_data(p3))
})
