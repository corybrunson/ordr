## signs of results are random
countries <- c(
  "Brazil", "Canada", "Netherlands", "New Zealand", "United States", "Zambia"
)
pop <- LifeCycleSavings[countries, 2:3]
oec <- LifeCycleSavings[countries, -(2:3)]
cc1 <- cancor(pop, oec)
cc2 <- unclass(candisc::cancor(pop, oec))

# cancor

x <- pop
y <- oec
xcenter = TRUE
ycenter = TRUE

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
    xcenter <- colMeans(x, )
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

# candisc::cancor

x <- pop
y <- oec
xcenter = TRUE
ycenter = TRUE
prefix = c("Xcan", "Ycan")

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
structure <- can.structure(X, Y, scores, use = use)
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
