
set.seed(0)
x <- runif(12); y <- runif(12)

plot(x, y)
(pc <- pchulls(x, y, prop = seq(0, 1, .1)))
for (i in unique(pc$hull)) {
  polygon(pc[pc$hull == i, , drop = FALSE], lwd = 2)
}

plot(x, y)
(pc <- pchulls(x, y, prop = seq(0, 1, .1), cut = "below"))
for (i in unique(pc$hull)) {
  polygon(pc[pc$hull == i, , drop = FALSE], lwd = 2)
}
