#' Reconstruct approximations to original data from ordination factors

#' @name reconstruct
#' @param x A bibble, or an object convertible to one.

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
