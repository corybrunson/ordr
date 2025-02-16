set.seed(284755L)
d <- data.frame(
  x = rnorm(12, mean = 3, sd = 1),
  y = rnorm(12, mean = 4, sd = 1)
)
p <- ggplot(d, aes(x, y))
f <- function(y) data.frame(
  y = median(y),
  ymin = quantile(y, .25),
  ymax = quantile(y, .75)
)

test_that("`geom_lineranges()` alerts when no function is provided", {
  expect_message(print(p + geom_lineranges()), regexp = "function")
})

test_that("`geom_lineranges()` computes summary values correctly", {
  
  # default: mean and standard error
  p1 <- p + geom_lineranges()
  d1 <- layer_data(p1)
  m1 <- sapply(d, mean)
  s1 <- sapply(d, \(z) sd(z) / sqrt(length(z)))
  r1 <- c(
    x = m1[["x"]], xmin = m1[["x"]] - s1[["x"]], xmax = m1[["x"]] + s1[["x"]],
    y = m1[["y"]], ymin = m1[["y"]] - s1[["y"]], ymax = m1[["y"]] + s1[["y"]]
  )
  expect_equal(unlist(d1[names(r1)]), r1)
  
  # custom: median and IQR
  p2 <- p + geom_lineranges(fun.data = f)
  d2 <- layer_data(p2)
  q2 <- lapply(d, quantile, probs = c(.5, .25, .75))
  r2 <- c(
    setNames(q2$x, c("x", "xmin", "xmax")),
    setNames(q2$y, c("y", "ymin", "ymax"))
  )
  expect_equal(unlist(d2[names(r2)]), r2)
  
})

d0 <- data.frame(
  a = 1, al = -1, au = 3,
  b = 2, bl = 1, bu = 3
)

test_that("`geom_pointranges()` pairs with identity stat", {
  p0 <- ggplot(d0, aes(a, b)) + 
    geom_pointranges(stat = "identity",
                     aes(xmin = al, xmax = au, ymin = bl, ymax = bu))
  l0 <- layer_data(p0)
  expect_equal(
    unname(l0[, c("x", "xmin", "xmax", "y", "ymin", "ymax")]),
    unname(d0)
  )
})
