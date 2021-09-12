## signs of results are random
countries <- c(
  "Brazil", "Canada", "Netherlands", "New Zealand", "United States", "Zambia"
)
pop <- LifeCycleSavings[countries, 2:3]
oec <- LifeCycleSavings[countries, -(2:3)]
cc1 <- cancor(pop, oec)
cc2 <- unclass(candisc::cancor(pop, oec))

# `stats::cancor()`
# comments follow ter Braak (1990)

x <- pop
y <- oec
xcenter = TRUE
ycenter = TRUE
xscale = TRUE
yscale = TRUE

x <- as.matrix(x)
y <- as.matrix(y)
if ((nr <- nrow(x)) != nrow(y)) 
  stop("unequal number of rows in 'cancor'")
ncx <- ncol(x)
ncy <- ncol(y)
if (!nr || !ncx || !ncy) 
  stop("dimension 0 in 'x' or 'y'")
if (is.logical(xcenter)) {
  if (xcenter) {
    xcenter <- colMeans(x)
    x <- x - rep(xcenter, rep.int(nr, ncx))
  }
  else xcenter <- rep.int(0, ncx)
} else {
  xcenter <- rep_len(xcenter, ncx)
  x <- x - rep(xcenter, rep.int(nr, ncx))
}
if (is.logical(ycenter)) {
  if (ycenter) {
    ycenter <- colMeans(y)
    y <- y - rep(ycenter, rep.int(nr, ncy))
  }
  else ycenter <- rep.int(0, ncy)
} else {
  ycenter <- rep_len(ycenter, ncy)
  y <- y - rep(ycenter, rep.int(nr, ncy))
}
# also added to `candisc::cancor`
if (xscale) {
  xscale <- apply(x, 2L, f)
  x <- sweep(x, 2L, xscale, "/", check.margin = FALSE)
}
if (yscale) {
  yscale <- apply(y, 2L, f)
  y <- sweep(y, 2L, yscale, "/", check.margin = FALSE)
}
qx <- qr(x)
qy <- qr(y)
dx <- qx$rank
if (!dx) 
  stop("'x' has rank 0")
dy <- qy$rank
if (!dy) 
  stop("'y' has rank 0")
z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx, , 
                                                drop = FALSE], dx, dy)
xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u)
rownames(xcoef) <- colnames(x)[qx$pivot][1L:dx]
ycoef <- backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v)
rownames(ycoef) <- colnames(y)[qy$pivot][1L:dy]
list(cor = z$d, xcoef = xcoef, ycoef = ycoef, xcenter = xcenter, 
     ycenter = ycenter)

# `candisc::cancor()`
# comments follow ter Braak (1990)

x <- pop
y <- oec
X.names = colnames(x)
Y.names = colnames(y)
row.names = rownames(x)
xcenter = TRUE
ycenter = TRUE
xscale = TRUE # not default
yscale = TRUE # not default
set.names = c("X", "Y")
prefix = c("Xcan", "Ycan")
na.rm = TRUE
use = if (na.rm) "complete" else "pairwise"
method = "gensvd"

# ADDED IN
if (xcenter) {
  xcenter <- colMeans(x)
  x <- x - rep(xcenter, rep.int(nrow(x), ncol(x)))
}
if (ycenter) {
  ycenter <- colMeans(y)
  y <- y - rep(ycenter, rep.int(nrow(y), ncol(y)))
}
f <- function(v) {
  v <- v[!is.na(v)]
  #sqrt(sum(v^2) / max(1, length(v) - 1L))
  #sqrt(sum(v^2) / max(1, length(v)))
  sqrt(sum(v^2))
}
if (xscale) {
  xscale <- apply(x, 2L, f)
  x <- sweep(x, 2L, xscale, "/", check.margin = FALSE)
}
if (yscale) {
  yscale <- apply(y, 2L, f)
  y <- sweep(y, 2L, yscale, "/", check.margin = FALSE)
}

X <- as.matrix(x) # X (n,p)
Y <- as.matrix(y) # Y (n,q)
p <- ncol(X)
q <- ncol(Y)
n <- length(complete.cases(X,Y))  # DONE: take account of 0 weights
#if(!missing(weights)) n <- n - sum(weights==0)

C <- candisc:::Var(cbind(X, Y), na.rm = TRUE, use=use)
Cxx <- C[1:p, 1:p]
Cyy <- C[-(1:p), -(1:p)]
Cxy <- C[1:p, -(1:p)]

ndim = min(p,q)
res <- candisc:::gensvd(Cxy, Cxx, Cyy, nu=ndim, nv=ndim)
names(res) <- c("cor", "xcoef", "ycoef")
colnames(res$xcoef) <- paste(prefix[1], 1:ndim, sep="")
colnames(res$ycoef) <- paste(prefix[2], 1:ndim, sep="")

scores <- candisc:::can.scores(X, Y, res$xcoef, res$ycoef)
colnames(scores$xscores) <- paste(prefix[1], 1:ndim, sep="")
colnames(scores$yscores) <- paste(prefix[2], 1:ndim, sep="")

structure <- candisc:::can.structure(X, Y, scores, use=use)
result <- list(cancor = res$cor, 
               names = list(X = X.names, Y = Y.names, 
                            row.names = row.names, set.names=set.names),
               ndim = ndim,
               dim = list(p=p, q=q, n=n),
               coef = list(X = res$xcoef, Y= res$ycoef),
               scores = list(X = scores$xscores, Y=scores$yscores),
               X = X, Y = Y, 
               #weights = if (missing(weights)) NULL else weights,
               structure = structure)
class(result) <- "cancor"
return(result)


X <- as.matrix(x)
Y <- as.matrix(y)
p <- ncol(X)
q <- ncol(Y)
ndim <- min(p, q)# DEFAULT
n <- length(complete.cases(X, Y))
#if (!missing(weights)) 
#  n <- n - sum(weights == 0)
C <- candisc:::Var(cbind(X, Y), na.rm = TRUE, use = "complete")
#C <- var(cbind(X, Y))
#C <- cor(cbind(X, Y))
Cxx <- C[1:p, 1:p]
Cyy <- C[-(1:p), -(1:p)]
Cxy <- C[1:p, -(1:p)]
res <- candisc:::gensvd(Cxy, Cxx, Cyy, nu = ndim, nv = ndim)
names(res) <- c("cor", "xcoef", "ycoef")
colnames(res$xcoef) <- paste(prefix[1], 1:ndim, sep = "")
colnames(res$ycoef) <- paste(prefix[2], 1:ndim, sep = "")
scores <- candisc:::can.scores(X, Y, res$xcoef, res$ycoef)
colnames(scores$xscores) <- paste(prefix[1], 1:ndim, sep = "")
colnames(scores$yscores) <- paste(prefix[2], 1:ndim, sep = "")
structure <- candisc:::can.structure(X, Y, scores, use = use)
result <- list(
  cancor = res$cor,
  names = list(
    X = X.names,
    Y = Y.names,
    row.names = row.names,
    set.names = set.names
  ), 
  ndim = ndim,
  dim = list(p = p, q = q, n = n),
  coef = list(
    X = res$xcoef, 
    Y = res$ycoef
  ),
  scores = list(
    X = scores$xscores, 
    Y = scores$yscores
  ),
  X = X,
  Y = Y,
  weights = if (missing(weights)) NULL else weights, 
  structure = structure
)
class(result) <- "cancor"
return(result)

# Greenacre (1984)
# A = xcoef, B = ycoef
# (4.4.8)
solve(Cxx) %*% Cxy %*% ycoef[, seq(2L)]
xcoef %*% diag(res$cor)
# (4.4.9)
solve(Cyy) %*% t(Cxy) %*% xcoef
ycoef[, seq(2L)] %*% diag(res$cor)

# ter Braak (1990)

# recover the correlation matrix
Cxy
cov(X) %*% res$xcoef %*% diag(res$cor) %*% t(res$ycoef) %*% t(cov(Y))

symm_sqrt <- function(x) {
  e <- eigen(x)
  e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
}
B_ <- symm_sqrt(Cxx) %*% xcoef %*% diag(res$cor)
C_ <- symm_sqrt(Cyy) %*% ycoef
B_ %*% t(C_[, -3L]) / Cxy

svd(solve(symm_sqrt(Cxx)) %*% Cxy %*% solve(symm_sqrt(Cyy)))
xcoef %*% diag(res$cor) %*% t(ycoef[, -3L])

# unexplained factor five
B__ <- Cxx %*% xcoef %*% diag(res$cor)
C__ <- Cyy %*% ycoef
B__ %*% t(C__[, -3L]) / Cxy


# following ter Braak, 1990
x <- pop
y <- oec
x <- as.matrix(x)
y <- as.matrix(y)
x <- sweep(x, 2L, apply(x, 2L, mean), "-")
y <- sweep(y, 2L, apply(y, 2L, mean), "-")
x <- sweep(x, 2L, apply(x, 2L, function(z) sqrt(sum(z^2))), "/")
y <- sweep(y, 2L, apply(y, 2L, function(z) sqrt(sum(z^2))), "/")
Rxx <- t(x) %*% x
Ryy <- t(y) %*% y
Ryx <- t(y) %*% x
symm_sqrt <- function(x) {
  e <- eigen(x)
  e$vectors %*% diag(sqrt(e$values)) %*% t(e$vectors)
}
plq <- svd(solve(symm_sqrt(Ryy)) %*% Ryx %*% solve(symm_sqrt(Rxx)))
# structure correlations
i <- 1
# interset correlations of criteria
b <- symm_sqrt(Ryy) %*% plq$u# %*% diag(plq$d ^ i)
# intraset correlations of predictors
c <- symm_sqrt(Rxx) %*% plq$v# %*% diag(plq$d ^ (1 - i))
# biplot of b and c closely approximates Ryx, the correlations between x and y
#b %*% t(c)

# recover B and C from `stats::cancor()` results
#Cyy %*% ycoef[, seq(length(z$d))] / b

# recover B and C from `candisc::cancor()` results
# Cyy = Ryy / (n - 1); Cyy * sqrt(n - 1) = Ryy / sqrt(n - 1)
((Ryy / sqrt(result$dim$n - 1)) %*% result$coef$Y) / b
(result$coef$Y / sqrt(result$dim$n - 1)) / (solve(Ryy) %*% b)
(((t(result$Y) %*% result$Y) / sqrt(result$dim$n - 1)) %*% result$coef$Y) / b
result$structure$Y.yscores / b
# Ryy / sqrt(n - 1) %*% ycoef = sqrt(Ryy) %*% P
# sqrt(Ryy) / sqrt(n - 1) %*% ycoef = P
((Rxx / sqrt(result$dim$n - 1)) %*% result$coef$X) / c
(((t(result$X) %*% result$X) / sqrt(result$dim$n - 1)) %*% result$coef$X) / c
result$structure$X.xscores / c

# what if `x` and `y` are not centered and scaled? what biplot should be used?
#symm_sqrt(solve(Cxx)) %*% Cxy %*% symm_sqrt(solve(Cyy))
#result$coef$X %*% diag(res$cor) %*% t(result$coef$Y)
#t(solve(chol(Cxx))) %*% Cxy %*% solve(chol(Cyy))

# structural correlations are different conferences of inertia
result$structure$X.yscores %*% t(result$structure$Y.yscores)
result$structure$X.xscores %*% t(result$structure$Y.xscores)
c %*% diag(plq$d) %*% t(b)

# mimic LDA? store type of biplot in *_ord object, have it control how factors
# * scores are supplemental points, as in LDA biplot
