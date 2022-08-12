# simulated biadditive model

# dimensions
p <- 3L; q <- 2L
# coefficients
a <- 2 ^ runif(p, min = -2, max = 2)
b <- 2 ^ runif(q, min = -2, max = 2)
# effects
xa <- a %*% t(rep(1L, q))
xb <- rep(1L, p) %*% t(b)
# error & noise
s <- .05
n <- rnorm(n = p * q, sd = s)
# entries
y <- xa + xb + matrix(n, p, q)
# bilinear (biadditive) model
m <- lm(as.vector(y) ~ offset(as.vector(xa)) + offset(as.vector(xb)))
print(m)
summary(m)
# residuals
r <- matrix(residuals(m), p, q)
# biplot
x <- log(y) - mean(log(y))
d <- svd(x)
plot(rbind(d$u, d$v), pch = NA, asp = 1)
points(x = 0, y = 0, pch = "+")
# segments(x0 = 0, y0 = 0, x1 = d$u[, 1L], y1 = d$u[, 2L], col = "blue")
# segments(x0 = 0, y0 = 0, x1 = d$v[, 1L], y1 = d$v[, 2L], col = "red")
points(d$u, pch = 16, col = "blue")
points(d$v, pch = 17, col = "red")
