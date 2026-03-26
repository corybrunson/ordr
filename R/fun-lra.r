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

#' @template ref-greenacre2010

#' @name lra-ord
#' @param x A numeric matrix or rectangular data set.
#' @param compositional Logical; whether to normalize rows of `x` to sum to 1.
#' @param weighted Logical; whether to weight rows and columns by their sums.
#' @param nd Integer; number of shared dimensions to include in print.
#' @param n Integer; number of rows of each factor to print.
#' @param main,var.axes,... Parameters passed to other plotting methods (in the
#'   case of `main`, after being [force()]d.
#' @param choices Integer; length-2 vector specifying the components to plot.
#' @param scale Numeric; values between 0 and 1 that control how inertia is
#'   conferred unto the points: Row (`i = 1L`) and column (`i = 2L`) coordinates
#'   are scaled by `sv ^ scale[[i]]`. If a single value `scale` is passed, it is
#'   assigned to the rows while `1 - scale` is assigned to the columns.

#' @return Given an \eqn{n * p} data matrix and setting \eqn{r=min(n,p)},
#'   `lra()` returns a list of class `"lra"` containing three elements:
#' \describe{
#'   \item{`sv`}{The \eqn{r-1} singular values}
#'   \item{`row.coords`}{The \eqn{n * (r-1)} matrix
#'                       of row standard coordinates.}
#'   \item{`column.coords`}{The \eqn{p * (r-1)} matrix
#'                          of column standard coordinates.}
#'   \item{`row.weights`}{The weights used to scale the row coordinates.}
#'   \item{`column.weights`}{The weights used to scale the column coordinates.}
#' }

#' @example inst/examples/ex-fun-lra-arrests.r
NULL

#' @rdname lra-ord
#' @export
lra <- function(x, compositional = FALSE, weighted = TRUE) {
  x <- as.matrix(x)
  # -+- save `.$row.sums` to output -+-
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
  u <- diag(1 / sqrt(as.vector(r))) %*% z$u[, -ncol(z$u), drop = FALSE]
  v <- diag(1 / sqrt(as.vector(c))) %*% z$v[, -ncol(z$v), drop = FALSE]
  dimnames(u) <- list(rownames(x), paste0("LRSV", seq(ncol(u))))
  dimnames(v) <- list(colnames(x), paste0("LRSV", seq(ncol(v))))
  res <- list(
    sv = z$d[seq(length(z$d) - 1L)],
    row.coords = u,
    column.coords = v,
    row.weights = as.vector(r),
    column.weights = as.vector(c)
  )
  structure(res, class = "lra")
}

#' @rdname lra-ord
#' @export
print.lra <- function (x, nd = length(x$sv), n = 6L, ...) {
  force(nd)
  rk <- length(x$sv)
  if (is.null(nd)) nd <- rk else if (nd > rk) {
    warning("'nd' is greater than the rank ", rk, " of the LRA.")
    nd <- rk
  } else if (nd <= 0) {
    warning("'nd' must be a positive integer.")
  }
  cat("Singular values:", format(x$sv, ...), "\n")
  rn <- nrow(x$row.coords)
  cn <- nrow(x$column.coords)
  rc <- x$row.coords[seq(min(rn, n)), seq(nd), drop = FALSE]
  cc <- x$column.coords[seq(min(cn, n)), seq(nd), drop = FALSE]
  cat("\nRow scores",if (n<rn) paste0(" (",n," of ",rn,")"),":\n", sep = "")
  print(rc)
  cat("\nColumn scores",if (n<cn) paste0(" (",n," of ",cn,")"),":\n", sep = "")
  print(cc)
  invisible(x)
}

#' @rdname lra-ord
#' @export
screeplot.lra <- function(x, main = deparse1(substitute(x)), ...) {
  force(main)
  names(x)[match("sv", names(x))] <- "sdev"
  screeplot.default(x, main = main, ...)
  invisible()
}

#' @rdname lra-ord
#' @export
biplot.lra <- function(
    x, choices = c(1L, 2L), scale = c(0, 0),
    main = deparse1(substitute(x)), var.axes = FALSE, ...
) {
  force(main)
  d <- x$sv
  rc <- x$row.coords
  cc <- x$column.coords
  if (length(choices) != 2L) stop("Length of `choices` must be 2")
  if (length(scale) == 1L) scale <- c(scale, 1 - scale)
  if (length(scale) != 2L) stop("Length of `scale` must be 1 or 2")
  if (any(scale < 0 | scale > 1)) warning("`scale` is outside [0, 1]")
  biplot.default(
    t(t(rc[, choices]) * d^scale[[1L]]),
    t(t(cc[, choices]) * d^scale[[2L]]),
    main = main, var.axes = var.axes, ...
  )
  invisible()
}

#' @rdname lra-ord
#' @export
plot.lra <- function(x, main = deparse1(substitute(x)), ...) {
  screeplot.lra(x, main = main, ...)
}
