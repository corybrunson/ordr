devtools::load_all()

# biplot following Greenacre
data(simple_example)
fortify_example <- fortify(simple_example)
# exhibit 1.4
plot(
  x = c(), y = c(),
  xlim = range(fortify_example$V1), ylim = range(fortify_example$V2),
  asp = 1
)
points(simple_example$u, pch = 19)
arrows(0, 0, simple_example$v$V1, simple_example$v$V2, col = "darkred", lwd = 2)
# isolines through y_1
y1 <- unlist(simple_example$v[1, ])
y1_unit <- y1 / sum(y1 ^ 2)
y1_min <- -15
y1_max <- 20
m <- y1[2] / y1[1]
coef <- c(0, m)
abline(a = 0, b = m, col = "darkred", lty = 2)
for (i in seq(y1_min, y1_max, by = 5)) {
  abline(a = i * y1_unit[2] - (i * y1_unit[1]) * (-1/m), b = (-1/m), lty = 2)
}
