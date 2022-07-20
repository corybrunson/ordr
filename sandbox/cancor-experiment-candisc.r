x <- LifeCycleSavings[, c("pop15", "pop75")]
y <- LifeCycleSavings[, c("sr", "dpi", "ddpi")]
nc <- 2L
x <- glass_banias[, seq(nc)]
y <- glass_banias_minor[, seq(nc)]
y$TiO2[[1L]] <- 0.5

# pre-processing
X <- as.matrix(x)
Y <- as.matrix(y)
p <- ncol(X)
q <- ncol(Y)
# covariance matrix
C <- cov(cbind(X, Y))
Cxx <- C[1:p, 1:p]
Cyy <- C[-(1:p), -(1:p)]
Cxy <- C[1:p, -(1:p)]
ndim = min(p, q)
# generalized SVD
Cxxinv <- solve(chol(Cxx)) # = Rxinv * scale factor (different)
Cyyinv <- solve(chol(Cyy)) # = Ryinv * scale factor (different)
Dform <- t(Cxxinv) %*% Cxy %*% Cyyinv # = t(Qx) %*% Qy, up to sign
result <- svd(Dform, nu = p, nv = q) # = z, up to sign
values <- result$d # = cor
Xmat <- Cxxinv %*% result$u # = xcoef * scale factor (same)
Ymat <- Cyyinv %*% result$v # = xcoef * scale factor (same)

Cxxinv / Rxinv # 6.956362, 2.2911
Cyyinv / Ryinv # 6.990657-7.020394, 1.848128
Dform / ( t(Qx) %*% Qy )
Xmat / xcoef # 7, 2.236068 = sqrt(5)
Ymat / ycoef # 7, 2.236068 = sqrt(5)

t(Rx) %*% Rx / Cxx # 49, 5
t(Ry) %*% Ry / Cyy # 49, 5
Xmat / (xcoef * sqrt(nrow(x) - 1L))
Ymat / (ycoef * sqrt(nrow(y) - 1L))

# `candisc::cancor()`
function (
    x, y, weights, X.names = colnames(x), Y.names = colnames(y), 
    row.names = rownames(x), xcenter = TRUE, ycenter = TRUE, 
    xscale = FALSE, yscale = FALSE, ndim = min(p, q), set.names = c("X", "Y"),
    prefix = c("Xcan", "Ycan"), na.rm = TRUE,
    use = if (na.rm) "complete" else "pairwise", 
    method = "gensvd", ...
) {
  X <- as.matrix(x)
  Y <- as.matrix(y)
  p <- ncol(X)
  q <- ncol(Y)
  n <- length(complete.cases(X, Y))
  if (!missing(weights)) 
    n <- n - sum(weights == 0)
  C <- candisc:::Var(cbind(X, Y), na.rm = TRUE, use = use, weights = weights)
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
      X = X.names, Y = Y.names,
      row.names = row.names, set.names = set.names
    ), 
    ndim = ndim,
    dim = list(p = p, q = q, n = n),
    coef = list(X = res$xcoef, Y = res$ycoef),
    scores = list(X = scores$xscores, Y = scores$yscores),
    X = X, Y = Y,
    weights = if (missing(weights)) NULL else weights, 
    structure = structure
  )
  class(result) <- "cancor"
  return(result)
}

# `candisc:::gensvd()`
function (Rxy, Rxx, Ryy, nu = p, nv = q) 
{
  p <- dim(Rxy)[1]
  q <- dim(Rxy)[2]
  if (missing(Rxx)) 
    Rxx <- diag(p)
  if (missing(Ryy)) 
    Ryy <- diag(q)
  if (dim(Rxx)[1] != dim(Rxx)[2]) 
    stop("Rxx must be square")
  if (dim(Ryy)[1] != dim(Ryy)[2]) 
    stop("Ryy must be square")
  s <- min(p, q)
  if (max(abs(Rxx - t(Rxx)))/max(abs(Rxx)) > 1e-10) {
    warning("Rxx not symmetric.")
    Rxx <- (Rxx + t(Rxx))/2
  }
  if (max(abs(Ryy - t(Ryy)))/max(abs(Ryy)) > 1e-10) {
    warning("Ryy not symmetric.")
    Ryy <- (Ryy + t(Ryy))/2
  }
  Rxxinv <- solve(chol(Rxx))
  Ryyinv <- solve(chol(Ryy))
  Dform <- t(Rxxinv) %*% Rxy %*% Ryyinv
  if (p >= q) {
    result <- svd(Dform, nu = nu, nv = nv)
    values <- result$d
    Xmat <- Rxxinv %*% result$u
    Ymat <- Ryyinv %*% result$v
  }
  else {
    result <- svd(t(Dform), nu = nv, nv = nu)
    values <- result$d
    Xmat <- Rxxinv %*% result$v
    Ymat <- Ryyinv %*% result$u
  }
  gsvdlist <- list(values = values, Xmat = Xmat, Ymat = Ymat)
  return(gsvdlist)
}

# `candisc:::can.scores()`
function (X, Y, xcoef, ycoef) 
{
  X.aux = scale(X, center = TRUE, scale = FALSE)
  Y.aux = scale(Y, center = TRUE, scale = FALSE)
  X.aux[is.na(X.aux)] = 0
  Y.aux[is.na(Y.aux)] = 0
  xscores = X.aux %*% xcoef
  yscores = Y.aux %*% ycoef
  return(list(xscores = xscores, yscores = yscores))
}

# `candisc:::can.structure()`
function (X, Y, scores, use = "complete.obs") 
{
  xscores <- scores$xscores
  yscores <- scores$yscores
  X.xscores = cor(X, xscores, use = use)
  Y.xscores = cor(Y, xscores, use = use)
  X.yscores = cor(X, yscores, use = use)
  Y.yscores = cor(Y, yscores, use = use)
  return(list(X.xscores = X.xscores, Y.xscores = Y.xscores, 
              X.yscores = X.yscores, Y.yscores = Y.yscores))
}
