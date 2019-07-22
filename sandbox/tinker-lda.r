# resources
# https://sebastianraschka.com/Articles/2014_python_lda.html
# https://stats.stackexchange.com/a/110733/68743
# other implementations
# https://github.com/vqv/ggbiplot/blob/experimental/R/fortify-lda.R
# https://github.com/fawda123/ggord/blob/master/R/ggord.R
# https://stackoverflow.com/questions/40121516/lda-contribution-biplot

# TASK: WRITE OUT EACH PROCESS ALGEBRAICALLY

# manually, following https://sebastianraschka.com/Articles/2014_python_lda.html

# data matrix: n * m
X <- as.matrix(iris[, 1:4])
# center `X`
X <- as.matrix(scale(X, center = TRUE, scale = FALSE))
# number of cases
n <- nrow(X)
# data mean: 1 * m
M <- t(apply(X, 2, mean))
# class assignments
C <- iris[, 5]
# class means: c * m
Mc <- as.matrix(aggregate(X, list(C), mean)[, -1])
# number of classes
c <- nrow(Mc)
# numbers of cases in each class
nc <- table(C)
# within-class scatter matrix: m * m
Sw <- t(X - Mc[C, ]) %*% (X - Mc[C, ])
# between-class scatter matrix: m * m
Sb <- t((Mc - M[rep(1, c), ])[C, ]) %*% (Mc - M[rep(1, c), ])[C, ]
# solve for Sw^-1 * Sb
E <- eigen(solve(Sw) %*% Sb)
# variance decomposition (centroids)
(E$values / sum(E$values))
# largest eigenvectors
W <- E$vectors
# rank of LDA
r <- 2
# transform data to get scores
U <- X %*% W[, 1:r]
# transpose inverse of transform to get loadings (as in SVD)
V <- t(solve(W))[, 1:r]
# biplot
plot(x = NA, y = NA, asp = 1,
     xlim = range(c(U[, 1], V[, 1])), ylim = range(c(U[, 2], V[, 2])))
points(0, 0, pch = 19)
points(U, col = as.integer(C), pch = (0:2)[as.integer(C)])
points(Mc %*% W[, 1:r], col = 1:c, pch = (15:17)[1:c], cex = 2)
segments(x0 = 0, y0 = 0, x1 = V[, 1], y1 = V[, 2], pch = 15, col = 4:7)

# manually, following Greenacre (2010)

# data matrix: I * J
X <- as.matrix(iris[, 1:4])
# number of cases
I <- nrow(X)
J <- ncol(X)
# class assignments
gx <- iris[, 5]
# class means: G * J
X_bar <- as.matrix(aggregate(X, list(gx), mean)[, -1])
# number of classes
G <- nrow(X_bar)
# within-class covariance matrix (w_ig = 1/I, w_g = 1/N_g)
C <- (1/I) * t(X - X_bar[gx, ]) %*% (X - X_bar[gx, ])
# centered class means (2 methods)
Y_bar <- X_bar - matrix(1/3, G, G) %*% X_bar
x_bar <- apply(X, 2, mean)
Y_bar <- X_bar - matrix(1, G, 1) %*% t(x_bar)
# eigendecomposition of within-class covariance matrix
Ceigen <- eigen(C)
# inverse of symmetric square root of within-class covariance matrix
Csqrtinv <- Ceigen$vectors %*% diag(1/sqrt(Ceigen$values)) %*% t(Ceigen$vectors)
# weighted Mahalanobis distance
S <- diag(sqrt(1/G), G) %*% Y_bar %*% Csqrtinv * 1/sqrt(J)
# singular value decomposition of weighted Mahalanobis distance
Ssvd <- svd(S)
# decomposition of variance (centroids)
(Ssvd$d^2 / sum(Ssvd$d^2))
# principal coordinates of group centroids
F <- diag(1/sqrt(1/G), G) %*% Ssvd$u %*% diag(Ssvd$d)
F <- Y_bar %*% Csqrtinv %*% Ssvd$v * sqrt(1/J)
# coordinates of variables for contribution biplot
Gamma <- Ssvd$v
# centered case data
Y <- X - t(x_bar)[rep(1, I), ]
# principal coordinates of cases
Fcase <- Y %*% Csqrtinv %*% Ssvd$v * sqrt(1/J)
# contribution biplot
plot(x = NA, y = NA, asp = 1,
     xlim = range(c(F[, 1], Gamma[, 1])), ylim = range(c(F[, 2], Gamma[, 2])))
points(0, 0, pch = 19)
points(Fcase, col = as.integer(gx), pch = (0:2)[as.integer(gx)])
points(F, col = 1:G, pch = (15:17)[1:G], cex = 2)
segments(x0 = 0, y0 = 0, x1 = Gamma[, 1], y1 = Gamma[, 2], pch = 15, col = 4:7)

# `MASS::lda()` and `ggbiplot::ggbiplot()`

ggbiplot::ggbiplot(MASS::lda(Species ~ ., iris))
# are the variable vectors based on cases rather than group centroids?

# simplified `MASS::lda()`

x <- iris[, 1:4]
grouping <- iris[, 5]
tol <- 1e-04

x <- as.matrix(x)
n <- nrow(x)
p <- ncol(x)
g <- as.factor(grouping)
lev <- lev1 <- levels(g)
counts <- as.vector(table(g))
proportions <- counts/n
ng <- length(proportions)
names(proportions) <- names(counts) <- lev1
# GROUP CENTROIDS
group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
# WITHIN-GROUP PRECISIONS (STANDARD DEVIATION INVERSES)
# "PRIOR" SYMMETRIC SQUARE ROOT OF COVARIANCE MATRIX
scaling <- diag(1/sqrt(diag(var(x - group.means[g, ]))))
scaling0 <- scaling
# (other methods)
# STANDARDIZED (CENTERED AND SCALED) AND SCALE-FACTORED DATA
X <- sqrt(1/(n - ng)) * (x - group.means[g, ]) %*% scaling
# RIGHT-SINGULAR VECTORS
X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol)
if (rank == 0L) 
  stop("rank = 0: variables are numerically constant")
if (rank < p) 
  warning("variables are collinear")
# 
scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank], , rank)
scaling1 <- scaling
# (resume)
# DATA CENTROID
xbar <- colSums(proportions %*% group.means)
# UNIFORM WEIGHTS
X <-
  sqrt((n * proportions) * 1/(ng - 1)) *
  # BETWEEN-GROUP DIFFERENCES
  scale(group.means, center = xbar, scale = FALSE) %*%
  scaling
X.s <- svd(X, nu = 0L)
# decomposition of variance (centroids)
(X.s$d^2 / sum(X.s$d^2))
rank <- sum(X.s$d > tol * X.s$d[1L])
if (rank == 0L) 
  stop("group means are numerically identical")
scaling <- scaling %*% X.s$v[, 1L:rank]
if (is.null(dimnames(x))) {
  dimnames(scaling) <- list(NULL, paste("LD", 1L:rank, sep = ""))
} else {
  dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank, sep = ""))
  dimnames(group.means)[[2L]] <- colnames(x)
}
cl <- match.call()
cl[[1L]] <- as.name("lda")
list(
  prior = proportions, counts = counts, means = group.means,
  scaling = scaling, lev = lev, svd = X.s$d[1L:rank], N = n
)

# simplified `MASS:::predict.lda()`

object <- MASS:::lda(x = x, grouping = grouping, tol = tol)

# input
prior <- object$prior
method <- "plug-in"
newdata <- eval.parent(object$call$x)
x <- as.matrix(newdata)
ng <- length(object$prior)
means <- colSums(prior * object$means)
scaling <- object$scaling
# center data on data centroid & scale to discriminant functions
x <- scale(x, center = means, scale = FALSE) %*% scaling
# center means on data centroid & scale to discriminant functions
dm <- scale(object$means, center = means, scale = FALSE) %*% scaling
dimen <- length(object$svd)
N <- object$N

# `method == "plug-in"`
dm <- dm[, 1L:dimen, drop = FALSE]
dist <- matrix(
  0.5 * rowSums(dm^2) - log(prior), nrow(x), length(prior), byrow = TRUE
) - x[, 1L:dimen, drop = FALSE] %*% t(dm)
dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))

# `method == "debiased"`
dm <- dm[, 1L:dimen, drop = FALSE]
dist <- matrix(0.5 * rowSums(dm^2), nrow(x), ng, byrow = TRUE) - 
  x[, 1L:dimen, drop = FALSE] %*% t(dm)
dist <- (N - ng - dimen - 1)/(N - ng) * dist -
  matrix(log(prior) - dimen/object$counts, nrow(x), ng, byrow = TRUE)
dist <- exp(-(dist - apply(dist, 1L, min, na.rm = TRUE)))

# other `method`
dist <- matrix(0, nrow = nrow(x), ncol = ng)
p <- ncol(object$means)
X <- x * sqrt(N/(N - ng))
for (i in 1L:ng) {
  nk <- object$counts[i]
  dev <- scale(X, center = dm[i, ], scale = FALSE)
  dev <- 1 + rowSums(dev^2) * nk/(N * (nk + 1))
  dist[, i] <- prior[i] * (nk/(nk + 1))^(p/2) * dev^(-(N - ng + 1)/2)
}

# output
posterior <- dist/drop(dist %*% rep(1, ng))
nm <- names(object$prior)
cl <- factor(nm[max.col(posterior)], levels = object$lev)
dimnames(posterior) <- list(rownames(x), nm)
list(class = cl, posterior = posterior, x = x[, 1L:dimen, drop = FALSE])

# `MASS::lda()` biplot

# parameters
x <- iris[, 1:4]
grouping <- iris[, 5]
tol <- 1e-04

# LDA
object <- MASS::lda(x = x, grouping = grouping, tol = tol)

# components
n <- object$N
ng <- length(object$lev)
means <- colSums(object$prior * object$means)
means_centered <- scale(object$means, center = means, scale = FALSE)
U <- means_centered %*% object$scaling
x_centered <- scale(eval.parent(object$call$x), center = means, scale = FALSE)
#U_x <- predict(object)$x
U_x <- x_centered %*% object$scaling
# recover V, adapting Greenacre (off by a scale factor or two)
Cw <- var(eval.parent(object$call$x) -
            object$means[eval.parent(object$call$grouping), ])
Cweigen <- eigen(Cw)
Cwsqrt <- Cweigen$vectors %*% diag(sqrt(Cweigen$values)) %*% t(Cweigen$vectors)
V <- 1/sqrt(ncol(object$means)) * Cwsqrt %*% object$scaling
V <- 1/sqrt(ncol(object$means)) * object$scaling
V <- 1/sqrt(ncol(object$means)) * solve(scaling1) %*% object$scaling
V <- 1/sqrt(length(object$lev)) *
  solve(scaling1 %*% t(scaling1)) %*%
  object$scaling * 5
# standardize discriminant coordinates
V <- 1/sqrt(ncol(object$means)) *
  solve(scaling0) %*%
  object$scaling

# biplot

# for comparison with Greenacre plot
U[, 2] <- -U[, 2]
U_x[, 2] <- -U_x[, 2]
V[, 2] <- -V[, 2]
# biplot
plot(x = NA, y = NA, asp = 1,
     xlim = range(c(U[, 1], V[, 1])),
     ylim = range(c(U[, 2], V[, 2])))
points(0, 0, pch = 19)
points(U_x, col = as.integer(grouping), pch = (0:2)[as.integer(grouping)])
points(U, col = 1:ng, pch = (15:17)[1:ng], cex = 2)
segments(x0 = 0, y0 = 0, x1 = V[, 1], y1 = V[, 2], pch = 15, col = 4:7)
