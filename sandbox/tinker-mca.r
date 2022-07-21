df <- ucb_admissions
nf <- 2L
abbrev <- FALSE

class.ind <- function(cl) {
  n <- length(cl)
  cl <- as.factor(cl)
  x <- matrix(0, n, length(levels(cl)))
  x[(1L:n) + n * (unclass(cl) - 1)] <- 1
  dimnames(x) <- list(names(cl), levels(cl))
  x
}
n <- nrow(df)
p <- length(df)
# case by level matrix
G <- as.matrix(do.call(
  "data.frame",
  c(lapply(df, class.ind), check.names = FALSE)
))
# level totals; Dc = diag(n_1,...,n_k)
Dc <- drop((rep(1, n)) %*% G)
# normalize `G` by sqrt( #variables * level total )
# Winv = diag(1/sqrt(p*Dc)); X = Winv G
X <- t(t(G)/(sqrt(p * Dc)))
# SVD: X = U D V'
X.svd <- svd(X)
# dimensions excluding first (constant)
sec <- 1 + (1L:nf)
# coordinates of rows
# X V / p = U D V' V / p = U D / p
# (note: inertia is contained in `X`)
rs <- X %*% X.svd$v[, sec]/p
# coordinates of column vertices
# Winv V = diag(1/sqrt(p*Dc)) V
cs <- diag(1/(sqrt(p * Dc))) %*% X.svd$v[, sec]
# row weights
# U Dinv / p
fs <- X.svd$u[, sec]/rep(p * X.svd$d[sec], rep(n, nf))
# singular values
d = X.svd$d[sec]

# varnames
if (abbrev) unlist(lapply(df, levels)) else colnames(G)

# reconstruct `X` from output
u <- fs %*% diag(X.svd$d[sec]) * p
v <- sqrt(p) * diag(sqrt(Dc)) %*% cs
head(u %*% diag(d) %*% t(v))
head(X.svd$u[, sec] %*% diag(X.svd$d[sec]) %*% t(X.svd$v[, sec]))

# `MASS::mca()`
function (
    df, nf = 2, abbrev = FALSE
) {
  class.ind <- function(cl) {
    n <- length(cl)
    cl <- as.factor(cl)
    x <- matrix(0, n, length(levels(cl)))
    x[(1L:n) + n * (unclass(cl) - 1)] <- 1
    dimnames(x) <- list(names(cl), levels(cl))
    x
  }
  if (!all(unlist(lapply(df, is.factor)))) 
    stop("all variables must be factors")
  Call <- match.call()
  n <- nrow(df)
  p <- length(df)
  G <- as.matrix(do.call("data.frame", c(lapply(df, class.ind), 
                                         check.names = FALSE)))
  Dc <- drop((rep(1, n)) %*% G)
  X <- t(t(G)/(sqrt(p * Dc)))
  X.svd <- svd(X)
  sec <- 1 + (1L:nf)
  rs <- X %*% X.svd$v[, sec]/p
  cs <- diag(1/(sqrt(p * Dc))) %*% X.svd$v[, sec]
  fs <- X.svd$u[, sec]/rep(p * X.svd$d[sec], rep(n, nf))
  dimnames(rs) <- list(row.names(df), as.character(1L:nf))
  dimnames(fs) <- dimnames(rs)
  varnames <- if (abbrev) 
    unlist(lapply(df, levels))
  else colnames(G)
  dimnames(cs) <- list(varnames, as.character(1L:nf))
  structure(list(rs = rs, cs = cs, fs = fs, d = X.svd$d[sec], 
                 p = p, call = Call), class = "mca")
}

x <- MASS::mca(df, nf = nf)
MASS::eqscplot(x$cs, type = "n", xlab = "")
text(x$rs)
text(x$cs, labels = dimnames(x$cs)[[1L]])

# `MASS:::plot.mca()`
function (
    x, rows = TRUE, col, cex = par("cex"), ...
) {
  if (length(cex) == 1L) 
    cex <- rep(cex, 2L)
  dev.hold()
  on.exit(dev.flush())
  eqscplot(x$cs, type = "n", xlab = "", ...)
  if (missing(col)) {
    col <- par("col")
    if (!is.numeric(col)) 
      col <- match(col, palette())
    col <- c(col, col + 1L)
  }
  else if (length(col) != 2L) 
    col <- rep(col, length = 2L)
  if (rows) 
    text(x$rs, cex = cex[1L], col = col[1L])
  text(x$cs, labels = dimnames(x$cs)[[1L]], cex = cex[2L], 
       col = col[2L])
  invisible(x)
}
