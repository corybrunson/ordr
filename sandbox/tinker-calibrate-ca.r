# `stats:::prcomp.default()`

`%||%` <- stats:::`%||%`

x = as.matrix(iris[, seq(4)])
retx = TRUE
center = TRUE
scale. = TRUE
tol = NULL
rank. = 2

x <- scale(x, center = center, scale = scale.)
cen <- attr(x, "scaled:center")
sc <- attr(x, "scaled:scale")
n <- nrow(x)
p <- ncol(x)
k <- min(as.integer(rank.), n, p)
s <- svd(x, nu = 0, nv = k)
j <- seq_len(k)
s$d <- s$d/sqrt(max(1, n - 1))
dimnames(s$v) <- list(colnames(x), paste0("PC", j))
r <- list(
  sdev = s$d,
  rotation = s$v,
  center = cen %||% FALSE, scale = sc %||% FALSE
)
r$x <- x %*% s$v
class(r) <- "prcomp"
r

# Task: Revert axis labels to variable values.

# zero on column standard axis
cz <- matrix(rep(0, nrow(r$rotation)))
# unit on column standard axis
cu <- matrix(rep(1, nrow(r$rotation)))
# scale & center zeros & units
matrix(cen) + matrix(1 / sc) * cz
matrix(cen) + matrix(1 / sc) * cu

# `MASS:::corresp.matrix()`

x = as.matrix(xtabs(breaks ~ ., warpbreaks))
nf = 1

# matrix total
N <- sum(x)
# row and column proportions
Dr <- drop(x %*% (rep(1/N, ncol(x))))
Dc <- drop((rep(1/N, nrow(x))) %*% x)
# observed - expected proportions
x1 <- x/N - outer(Dr, Dc)
# standardization factors (scale factors for chi^2 distances)
Dr <- 1/sqrt(Dr)
Dc <- 1/sqrt(Dc)
# SVD of standardized residuals matrix
X.svd <- svd(t(t(x1 * Dr) * Dc))
dimnames(X.svd$v) <- list(colnames(x), NULL)
dimnames(X.svd$u) <- list(rownames(x), NULL)
# output classed list
res <- list(
  # singular values; inertias
  cor = X.svd$d[1L:nf],
  # row standard coordinates
  rscore = X.svd$u[, 1L:nf] * Dr,
  # column standard coordinates
  cscore = X.svd$v[, 1L:nf] * Dc,
  # original matrix
  Freq = x
)
class(res) <- "correspondence"
res

# Task: Revert standardized residual axis labels to non-standardized residuals.

# with respect to first row
ri <- 1
# with respect to mean
# Drm <- 
# unit on column standardized residual axis
# cz <- rep(0, nrow(res$cscore))
cz <- rep(0, length(res$cscore))
cu <- rep(1, length(res$cscore))
# (reverse process) *** depends on what row the chi^2 distance is w.r.t.
(cz / Dc / Dr[ri] / Dc + outer(1/(Dr[ri]^2), 1/(Dc^2))) * sum(x)
(cu / Dc / Dr[ri] / Dc + outer(1/(Dr[ri]^2), 1/(Dc^2))) * sum(x)
