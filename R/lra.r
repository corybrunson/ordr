#' @title Log-ratio analysis
#' 
#' @description ...
#' 

#' @name lra
#' @param x A numeric matrix or rectangular data set.
#' @example inst/examples/arrests-logratio-polygon.r
NULL

#' @rdname lra
#' @export
lra <- function(x) {
  x <- as.matrix(x)
  r <- (1 / sum(x)) * x %*% matrix(1, ncol(x))
  c <- (1 / sum(x)) * t(x) %*% matrix(1, nrow(x))
  l <- log(x)
  y <- (diag(nrow(x)) - matrix(1, nrow(x)) %*% t(r)) %*%
    l %*%
    t(diag(ncol(x)) - matrix(1, ncol(x)) %*% t(c))
  d_r <- diag(1 / nrow(x), nrow(x))
  d_c <- diag(1 / ncol(x), ncol(x))
  s <- sqrt(d_r) %*% y %*% sqrt(d_c)
  dimnames(s) <- dimnames(x)
  d <- svd_ord(s)
  u <- d$u
  v <- d$v
  colnames(u) <- paste0("LRSV", 1:ncol(u))
  colnames(v) <- paste0("LRSV", 1:ncol(v))
  res <- list(
    sv = d$d,
    row.coords = u,
    column.coords = v
  )
  structure(res, class = "lra")
}
