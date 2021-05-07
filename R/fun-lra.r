#' @title Log-ratio analysis
#'
#' @description Represent log-ratios between variables based on their values on
#'   a population of cases.
#'
#' @details
#'
#' Log-ratio analysis (LRA) is based on a double-centering of log-transformed
#' data, usually weighted by row and column totals. The technique is suitable
#' for positive-valued variables on a common scale (e.g. percentages). The
#' distances between variables' coordinates (in the full-dimensional space) are
#' their pairwise log-ratios. The distances between cases' coordinates are
#' called their _log-ratio distances_, and the total variance is the weighted
#' sum of their squares.
#'
#' LRA is not implemented in standard R distributions but is a useful member of
#' the ordination toolkit. This is a minimal implementation following
#' Greenacre's (2010) exposition in Chapter 7.
#' 

#' @return Given an \eqn{n * p} data matrix and setting \eqn{r=min(n,p)},
#'   `lra()` returns a list of class `"lra"` containing three elements:
#' \itemize{
#'   \item{sv}{The \eqn{r-1} singular values}
#'   \item{row.coords}{The \eqn{n * (r-1)} matrix
#'                     of row standard coordinates.}
#'   \item{column.coords}{The \eqn{p * (r-1)} matrix
#'                        of column standard coordinates.}
#' }
#' 

#' @template ref-greenacre2010

#' @name lra-ord
#' @param x A numeric matrix or rectangular data set.
#' @param compositional Logical; whether to normalize rows of `x` to sum to 1.
#' @param weighted Logical; whether to weight rows and columns by their sums.
NULL

#' @rdname lra-ord
#' @export
lra <- function(x, compositional = FALSE, weighted = TRUE) {
  x <- as.matrix(x)
  if (compositional) {
    x <- sweep(x, 1, rowSums(x), "/")
  }
  n <- sum(x)
  if (weighted) {
    r <- x %*% matrix(1, ncol(x)) / n
    c <- t(x) %*% matrix(1, nrow(x)) / n
  } else {
    r <- matrix(1, nrow(x)) / nrow(x)
    c <- matrix(1, ncol(x)) / ncol(x)
  }
  y <- log(x)
  m_c <- t(y) %*% r
  y <- y - matrix(1, nrow(x)) %*% t(m_c)
  m_r <- y %*% c
  y <- y - m_r %*% t(matrix(1, ncol(x)))
  d_r <- diag(as.vector(r))
  d_c <- diag(as.vector(c))
  s <- sqrt(d_r) %*% y %*% sqrt(d_c)
  dimnames(s) <- dimnames(x)
  z <- svd(s)
  u <- diag(1/sqrt(as.vector(r))) %*% z$u[, -ncol(z$u), drop = FALSE]
  v <- diag(1/sqrt(as.vector(c))) %*% z$v[, -ncol(z$v), drop = FALSE]
  dimnames(u) <- list(rownames(x), paste0("LRSV", seq(ncol(u))))
  dimnames(v) <- list(colnames(x), paste0("LRSV", seq(ncol(v))))
  res <- list(
    sv = z$d[seq(length(z$d) - 1L)],
    row.coords = u,
    column.coords = v
  )
  structure(res, class = "lra")
}
