#' @title Reconstruct data from a tbl_ord
#'
#' @description This convenience function reconstructs an approximation to the
#'   (possibly transformed) original data matrix from the ordination factors and
#'   metadata. The methods are most useful as points of reference for
#'   understanding how ordinations are obtained from data.

#' @name reconstruct
#' @param x A [tbl_ord] object, or an object coercible to one.
NULL

#' @rdname reconstruct
#' @export
reconstruct <- function(x) UseMethod("reconstruct")

#' @rdname reconstruct
#' @export
reconstruct.list <- function(x) {
  res <- get_u(x) %*% t(get_v(x))
  if (!is.null(get_u(x)$name)) rownames(res) <- get_u(x)$name
  if (!is.null(get_v(x)$name)) colnames(res) <- get_v(x)$name
  res
}
