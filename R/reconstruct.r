#' Reconstruct data from an ordination
#'
#' This convenience function reconstructs an approximation to the (possibly
#' transformed) original data matrix from the ordination factors and metadata.
#' The methods are most useful as points of reference for understanding how
#' ordinations are obtained from data.

#' @name reconstruct
#' @param x A \code{\link{tbl_ord}}, or an object convertible to one.

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
