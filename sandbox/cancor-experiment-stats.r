# first data set
x <- LifeCycleSavings[, c("pop15", "pop75")]
y <- LifeCycleSavings[, c("sr", "dpi", "ddpi")]
# second data set
nc <- 2L
x <- glass_banias[, seq(nc)]
y <- glass_banias_minor[, seq(nc)]
y$TiO2[[1L]] <- 0.5

# pre-processing
x <- as.matrix(x)
y <- as.matrix(y)
stopifnot((nr <- nrow(x)) == nrow(y))
ncx <- ncol(x)
ncy <- ncol(y)
xcenter <- colMeans(x, )
x <- x - rep(xcenter, rep.int(nr, ncx))
ycenter <- colMeans(y)
y <- y - rep(ycenter, rep.int(nr, ncy))
# QR-decomposition
qx <- qr(x)
qy <- qr(y)
dx <- qx$rank
dy <- qy$rank
# SVD
z <- svd(qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx, , drop = FALSE], dx, dy)
xcoef <- backsolve((qx$qr)[1L:dx, 1L:dx, drop = FALSE], z$u)
ycoef <- backsolve((qy$qr)[1L:dy, 1L:dy, drop = FALSE], z$v)
# 
Rxinv <- solve((qx$qr)[1L:dx, 1L:dx, drop = FALSE])
Rxinv %*% z$u # = xcoef
Ryinv <- solve((qy$qr)[1L:dy, 1L:dy, drop = FALSE])
Ryinv %*% z$v # = ycoef
# 
Qx <- qr.Q(qx)
Rx <- qr.R(qx)
Qy <- qr.Q(qy)
Ry <- qr.R(qy)
cor(Qx) # Qx is orthogonal

nr - 1L # 49, 5
t(Rx) %*% Rx / cov(x) # 49, 5
t(Ry) %*% Ry / cov(y) # 49, 5

# 
identical(qr.qy(qy, diag(1, nr, dy)),
          Qy %*% diag(1, dy),
          Qy)
qr.qty(qx, qr.qy(qy, diag(1, nr, dy)))[1L:dx, , drop = FALSE] - t(Qx) %*% Qy
# z = svd( t(Qx) %*% Qy )

function (
    x, y, xcenter = TRUE, ycenter = TRUE
) {
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
  }
  else {
    xcenter <- rep_len(xcenter, ncx)
    x <- x - rep(xcenter, rep.int(nr, ncx))
  }
  if (is.logical(ycenter)) {
    if (ycenter) {
      ycenter <- colMeans(y)
      y <- y - rep(ycenter, rep.int(nr, ncy))
    }
    else ycenter <- rep.int(0, ncy)
  }
  else {
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
}
