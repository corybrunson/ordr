# resources
# https://github.com/cran/MASS/blob/master/R/lda.R
# https://tinyurl.com/yyhpyu2d [Venables & Ripley]
# https://sebastianraschka.com/Articles/2014_python_lda.html [Raschka]
# https://stats.stackexchange.com/a/110733/68743 [amoeba]
# https://stats.stackexchange.com/a/83114/68743 [ttnphns]
# https://stats.stackexchange.com/q/237217/68743
# other implementations
# https://github.com/vqv/ggbiplot/blob/experimental/R/fortify-lda.R
# https://github.com/fawda123/ggord/blob/master/R/ggord.R
# https://stackoverflow.com/questions/40121516/lda-contribution-biplot

set.seed(0)
iris <- iris[sample(nrow(iris), 60, replace = FALSE), ]
iris <- iris[order(iris[, 5]), ]

# 2D illustration [amoeba]
iris <- iris[, c(1:2, 5)]

# 
# manually, following https://sebastianraschka.com/Articles/2014_python_lda.html
# 

# data matrix: n * m
X <- as.matrix(iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))])
# center `X`
X <- as.matrix(scale(X, center = TRUE, scale = FALSE))
# number of cases
n <- nrow(X)
# data mean: 1 * m
M <- t(apply(X, 2, mean))
# class assignments
C <- iris[, "Species"]
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

# 
# manually, following Greenacre (2010)
# 

# data matrix: I * J
X <- as.matrix(iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))])
# number of cases
I <- nrow(X)
J <- ncol(X)
# class assignments
gx <- iris[, "Species"]
# class weights (counts)
ng <- aggregate(gx, list(gx), length)$x
# class means: G * J
X_bar <- as.matrix(aggregate(X, list(gx), mean)[, -1])
# number of classes
G <- nrow(X_bar)
# within-class covariance matrix (w_ig = 1/I, w_g = 1/N_g)
C <- (1/I) * t(X - X_bar[gx, ]) %*% (X - X_bar[gx, ])
# centered class means (2 methods)
Y_bar <- X_bar - matrix(ng/I, G, G, byrow = TRUE) %*% X_bar
x_bar <- apply(X, 2, mean)
Y_bar <- X_bar - matrix(1, G, 1) %*% t(x_bar)
# eigendecomposition of within-class covariance matrix
Ceigen <- eigen(C)
# inverse of symmetric square root of within-class covariance matrix
Csqrtinv <- Ceigen$vectors %*% diag(1/sqrt(Ceigen$values)) %*% t(Ceigen$vectors)
# weighted Mahalanobis distance
S <- diag(sqrt(1/ng)) %*% Y_bar %*% Csqrtinv * 1/sqrt(J)
# singular value decomposition of weighted Mahalanobis distance
Ssvd <- svd(S)
# decomposition of variance (centroids)
(Ssvd$d^2 / sum(Ssvd$d^2))
# principal coordinates of group centroids
F <- diag(1/sqrt(1/ng)) %*% Ssvd$u %*% diag(Ssvd$d)
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

# 
# `MASS::lda()` and `ggbiplot::ggbiplot()`
# 

ggbiplot::ggbiplot(MASS::lda(Species ~ ., iris))
# are the variable vectors based on cases rather than group centroids?

# 
# simplified `MASS::lda()` and `MASS::predict.lda()`
# 

# simplified `MASS::lda()`

# input
x <- iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))]
grouping <- iris[, "Species"]
tol <- 1e-04

# pre-
x <- as.matrix(x)
n <- nrow(x)
p <- ncol(x)
g <- as.factor(grouping)
lev <- lev1 <- levels(g)
counts <- as.vector(table(g))
proportions <- counts/n
ng <- length(proportions)
names(proportions) <- names(counts) <- lev1
# group centroids
## drop attributes to avoid e.g. matrix() methods
group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
# within-group precisions
# ("prior" symmetric square root of covariance matrix?)
## scale columns to unit variance before checking for collinearity
scaling <- diag(1/sqrt(diag(var(x - group.means[g, ]))))
scaling0 <- scaling
# NOTE: unit variances, but not identity correlation matrix
print(var((x - group.means[g, ]) %*% scaling))
# next code chunks are options to calculate sphering matrix

# `method == 'mve'`
## adjust to "unbiased" scaling of covariance matrix
cov <- n/(n - ng) * MASS::cov.rob((x - group.means[g, ]) %*% scaling)$cov
sX <- svd(cov, nu = 0L)
rank <- sum(sX$d > tol^2)
if (rank == 0L) 
  stop("rank = 0: variables are numerically constant")
if (rank < p) 
  warning("variables are collinear")
scaling <- scaling %*% sX$v[, 1L:rank] %*% diag(sqrt(1/sX$d[1L:rank]), , rank)
scaling1 <- scaling

# `method == 't'`
if (nu <= 2) 
  stop("'nu' must exceed 2")
w <- rep(1, n)
repeat {
  w0 <- w
  X <- x - group.means[g, ]
  sX <- svd(sqrt((1 + p/nu) * w/n) * X, nu = 0L)
  X <- X %*% sX$v %*% diag(1/sX$d, , p)
  w <- 1/(1 + drop(X^2 %*% rep(1, p))/nu)
  print(summary(w))
  group.means <- tapply(w * x, list(rep(g, p), col(x)), 
                        sum)/rep.int(tapply(w, g, sum), p)
  if (all(abs(w - w0) < 0.01)) 
    break
}
X <- sqrt(nu/(nu - 2) * (1 + p/nu)/n * w) * (x - group.means[g, ]) %*% scaling
X1 <- X
X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol)
if (rank == 0L) 
  stop("rank = 0: variables are numerically constant")
if (rank < p) 
  warning("variables are collinear")
scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank], , rank)
scaling1 <- scaling

# other `method`
# `t(X) %*% X` = `(n-1)/(n-ng) * R_W`, i.e. rescaled within-group correlation
X <-
  # alternatively `sqrt(1/n`
  sqrt(1/(n - ng)) *
  # within-group differences
  (x - group.means[g, ]) %*%
  scaling
X1 <- X
# RIGHT-SINGULAR VECTORS
# `X.s$d` = `sqrt((n-1)/(n-ng) * eigen(R_W)$values)`
# `X.s$v` = `eigen( t(X) %*% X )$vectors` = `eigen(R_W)$vectors`
X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol)
if (rank == 0L) 
  stop("rank = 0: variables are numerically constant")
if (rank < p) 
  warning("variables are collinear")
scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank], , rank)
scaling1 <- scaling

# NOTE: identity correlation matrix
print(var((x - group.means[g, ]) %*% scaling))
## now have variables scaled so that W is the identity
# maximize t(a) * S_B * a subject to |a| = 1
# data centroid
xbar <- colSums(proportions %*% group.means)
# 
X <-
  # alternatively `1/ng`
  sqrt((n * proportions) * 1/(ng - 1)) *
  # between-group differences
  scale(group.means, center = xbar, scale = FALSE) %*%
  scaling
X2 <- X
# eigendecomposition of `t(X2) %*% X2` = 1/(ng-1) * t(Sph) * S_B * Sph
# column vectors `v = X.s$v[, i]` maximize `t(v) %*% t(X2) %*% X2 %*% v`
X.s <- svd(X, nu = 0L)
# decomposition of variance (centroids)
(X.s$d^2 / sum(X.s$d^2))
rank <- sum(X.s$d > tol * X.s$d[1L])
if (rank == 0L) 
  stop("group means are numerically identical")
# unstandardized discriminant coefficients
scaling <- scaling %*% X.s$v[, 1L:rank]
scaling2 <- scaling

# output
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

# 
# `MASS::lda()` biplot
# 

# parameters
x <- iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))]
grouping <- iris[, "Species"]
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
x_within <- eval.parent(object$call$x) -
  object$means[eval.parent(object$call$grouping), ]
#U_x <- predict(object)$x
U_x <- x_centered %*% object$scaling
# recover V, adapting Greenacre (off by a scale factor or two)
Cw <- var(x_within)
Cweigen <- eigen(Cw)
Cwsqrt <- Cweigen$vectors %*% diag(sqrt(Cweigen$values)) %*% t(Cweigen$vectors)
# compare to Greenacre: `Cwsqrt` = symmetric square root of `C_W`
V <- Cwsqrt %*% object$scaling
V <- 1/sqrt(ncol(object$means)) *
  Cwsqrt %*% object$scaling
# standardize discriminant coefficients (by standard deviations of variables)
# standardized discriminant coefficients, from [ttnphns]
V <- cbind(
  LD1 = -c(-.4269548486, -.5212416758, .9472572487, .5751607719),
  LD2 = c(.0124075316, .7352613085, -.4010378190, .5810398645)
)
# compare to [ttnphns]: `scaling0` = 1/sqrt(diag(C_W))
# to a factor of `sqrt((n-1)/n)`
sqrt(1/n) * diag(sqrt(diag(S_W))) %*% object$scaling
V <- diag(1/diag(scaling0)) %*% object$scaling
# pooled within-group correlations
# pooled within-groups correlations, from [ttnphns]
V <- cbind(
  LD1 = -c(.2225959415, -.1190115149, .7060653811, .6331779262),
  LD2 = c(.3108117231, .8636809224, .1677013843, .7372420588)
)
cor(x_within, as.matrix(x_within) %*% object$scaling) # [ttnphns]
t(scale(x_within)) %*% (scale(as.matrix(x_within) %*% object$scaling)) / (n-1)
# `solve(scaling1) %*% object$scaling = X.s$v`
diag(1/diag(S_W)) %*% S_W %*% solve(scaling1) %*% object$scaling # fail

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

# 
# synthesis
# 

X <- as.matrix(iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))])
g <- as.matrix(as.integer(iris[, "Species"]))
n <- nrow(X); m <- ncol(X); q <- length(unique(g))
#x_bar <- apply(X, 2, mean)
x_bar <- 1/n * matrix(1, nrow = 1, ncol = n) %*% X
G <- outer(
  as.vector(g), sort(as.vector(unique(g))),
  function(x, y) as.integer(x == y)
)
X_hat <- X - matrix(1, nrow = n, ncol = 1) %*% x_bar
Q <- t(G) %*% G
Q_bar <- solve(Q) %*% t(G) %*% X
Y_hat <- X - G %*% Q_bar
Q_hat <- Q_bar - matrix(1, nrow = q, ncol = 1) %*% x_bar
Z_hat <- G %*% Q_hat
S_T <- t(X_hat) %*% X_hat
S_W <- t(Y_hat) %*% Y_hat
S_B <- t(Z_hat) %*% Z_hat
sum(S_W) + sum(S_B) - sum(S_T)
sum(S_B) / sum(S_W)
## compare to Raschka (Python article):
#eigen(solve(S_W) %*% S_B)
C_W <- 1/n * S_W
C_B <- 1/n * S_B
eig_W <- eigen(C_W)
E_W <- eig_W$vectors
D_W <- diag(eig_W$values)
Cinvsqrt_W <- E_W %*% diag(1/sqrt(diag(D_W))) %*% t(E_W)
V_W <- diag(diag(C_W))
Vinvsqrt_W <- diag(1/sqrt(diag(V_W)))
# compare to `scaling0`:
sqrt((n-1)/n) * Vinvsqrt_W
R_W <- Vinvsqrt_W %*% C_W %*% Vinvsqrt_W
eig_hat_W <- eigen(R_W)
E_hat_W <- eig_hat_W$vectors
D_hat_W <- diag(eig_hat_W$values)
D_hat_invsqrt_W <- diag(1/sqrt(diag(D_hat_W)))
# compare to `scaling1`:
sqrt((n-q)/n) *
  Vinvsqrt_W %*% E_hat_W %*% D_hat_invsqrt_W
# compare to `X2`:
sqrt((n-q)/n/(q-1)) *
  diag(sqrt(diag(Q))) %*% Q_hat %*%
  Vinvsqrt_W %*% E_hat_W %*% D_hat_invsqrt_W
eig_B <- eigen(C_B)
E_B <- eig_B$vectors
D_B <- diag(eig_B$values)
V_B <- diag(diag(C_B))
Vinvsqrt_B <- diag(1/sqrt(diag(V_B)))
R_B <- Vinvsqrt_B %*% C_B %*% Vinvsqrt_B

# key eigendecompositions
# Raschka:
eig_Raschka <- eigen(solve(S_W) %*% S_B)
eig_Raschka$values / sum(eig_Raschka$values)
eig_Raschka$vectors # [ttnphns]
# canonical correlations
sqrt(eig_Raschka$values / (eig_Raschka$values + 1)) # [ttnphns]
# Greenacre: Mahalanobis distances
# compare to `S`
M <- sqrt(1/(n*m)) * diag(sqrt(diag(Q))) %*% Q_hat %*% Cinvsqrt_W
# compare to `t(S) %*% S`
Mip <- 1/(n*m) * Cinvsqrt_W %*% S_B %*% Cinvsqrt_W
eig_Greenacre <- eigen(Mip)
# Raschka vs Greenacre:
eig_Greenacre$values / sum(eig_Greenacre$values)
eig_Greenacre$vectors
# MASS:
# compare to `t(X2) %*% X2`
X2alt <- sqrt((n-q)/(q-1)) *
  diag(sqrt(diag(D_B))) %*% t(E_B) %*%
  Vinvsqrt_W %*% E_hat_W %*% D_hat_invsqrt_W
X2alt <- X2alt[1:2, ]
(t(X2) %*% X2) / (t(X2alt) %*% X2alt)
svd_X2 <- svd(X2alt)
svd_X2$d^2 / sum(svd_X2$d^2)
# compare to `svd(X2, nu = 0)`:
svd_X2$d
svd_X2$v
# compare to `scaling2`:
Salt <- sqrt((n-q)/n) *
  Vinvsqrt_W %*% E_hat_W %*% D_hat_invsqrt_W %*%
  svd_X2$v

# ordination factors
# U (classes)
Q_hat %*% Salt
sqrt(n / (n - q)) / 2 * Q_hat %*% Salt / F[, 1:2]
# U (cases)
head(X_hat %*% Salt)
sqrt(n / (n - q)) / 2 * head(X_hat %*% Salt) / head(Fcase[, 1:2])
# V
E_W %*% diag(sqrt(diag(D_W))) %*% t(E_W) %*% Salt
# = C_W^1/2 * `object$scaling`
sqrt(n / (n - q)) *
  E_W %*% diag(sqrt(diag(D_W))) %*% t(E_W) %*% Salt / Gamma[, 1:2]




# setup for `lda()`
x <- iris[, grep("\\.([Ww]idth|[Ll]ength)", names(iris))]
grouping <- iris[, "Species"]
tol <- 1e-04
# start of `lda()`
x <- as.matrix(x)
n <- nrow(x)
p <- ncol(x)
g <- as.factor(grouping)
lev <- lev1 <- levels(g)
counts <- as.vector(table(g))
proportions <- counts/n
ng <- length(proportions)
names(proportions) <- names(counts) <- lev1
group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)

# original internals for `lda()`:
scaling <- diag(1/sqrt(diag(var(x - group.means[g, ]))))
X <- sqrt(1/(n - ng)) * (x - group.means[g, ]) %*% scaling
X.s <- svd(X, nu = 0L)
rank <- sum(X.s$d > tol)
scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank], , rank)

# alternate internals for `lda()`:
cov.w <- var(x - group.means[g, ])
scaling <- diag(1/sqrt(diag(cov.w)))
# eigendecompose `cov.w` and calculate `X.s$d` and `X.s$v` therefrom?
cov.eig <- eigen(cov.w)
# or just store `cov.eig` in addition to the other named elements
