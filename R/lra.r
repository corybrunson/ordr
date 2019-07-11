#' @title Log-ratio analysis
#'
#' @description Represent log-ratios between variables based on their values on
#'   a population of cases.
#'
#' @details Log-ratio analysis (LRA) is based on a double-centering of
#'   log-transformed data, usually weighted by row and column totals. The
#'   technique is suitable for positive-valued variables on a common scale (e.g.
#'   percentages). The distances between variables' coordinates (in the
#'   full-dimensional space) are their pairwise log-ratios. The distances
#'   between cases' coordinates are called their _log-ratio distances_, and the
#'   total variance is the weighted sum of their squares.
#'
#'   LRA is not implemented in standard R distributions but is a useful member
#'   of the ordination toolkit. This is a minimal implementation following
#'   Greenacre's (2010) exposition in Chapter 7.
#'   

#' @template ref-greenacre2010

#' @name lra
#' @param x A numeric matrix or rectangular data set.
#' @param weighted Logical; whether to weight rows and columns by their sums.
#' @example inst/examples/arrests-logratio-polygon.r
NULL

#' @rdname lra
#' @export
lra <- function(x, weighted = TRUE) {
  x <- as.matrix(x)
  if (weighted) {
    r <- (1 / sum(x)) * x %*% matrix(1, ncol(x))
    c <- (1 / sum(x)) * t(x) %*% matrix(1, nrow(x))
  } else {
    r <- (1 / nrow(x)) * matrix(1, nrow(x))
    c <- (1 / ncol(x)) * matrix(1, ncol(x))
  }
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
