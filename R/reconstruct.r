
reconstruct <- function(x) UseMethod("reconstruct")

reconstruct.bbl <- function(x) {
  res <- factor_u(x) %*% t(factor_v(x))
  if (!is.null(get_u(x)$name)) rownames(res) <- get_u(x)$name
  if (!is.null(get_v(x)$name)) colnames(res) <- get_v(x)$name
  res
}
