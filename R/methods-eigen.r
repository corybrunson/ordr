#' Functionality for matrix spectral decompositions
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"eigen"}. (This is a class introduced in this package to identify 
#' objects returned by \code{\link{eigen}}, which is masked by a wrapper that
#' adds the class attribute.)
#' 
#' @name methods-eigen
#' @template param-methods
#' @template param-matrix
#' @template param-align
# @example inst/examples/ex-eigen.r (uses *igraph*)

#' @rdname methods-eigen
#' @export
as_tbl_ord.eigen <- as_tbl_ord_default

#' @rdname methods-eigen
#' @export
reconstruct.eigen <- function(x) {
  x$vectors %*% diag(x$values) %*% t(x$vectors)
}

recover_uv_eigen <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  w <- 1:(min(which(c(x$values, -1) < 0)) - 1)
  res <- x$vectors[, w, drop = FALSE] %*%
    diag(sqrt(x$values[w]), nrow = length(w))
  #res <- sweep(x$vectors, 2, sqrt(x$values), "*")
  dimnames(res) <- list(
    dimnames(attr(x, "x"))[[switch(.matrix, u = 1, v = 2)]],
    recover_coord(x)
  )
  res
}

#' @rdname methods-eigen
#' @export
recover_u.eigen <- function(x) recover_uv_eigen(x, "u")

#' @rdname methods-eigen
#' @export
recover_v.eigen <- function(x) recover_uv_eigen(x, "v")

#' @rdname methods-eigen
#' @export
recover_inertia.eigen <- function(x) {
  x$values[1:(min(which(c(x$values, -1) < 0)) - 1)] ^ 2
}

#' @rdname methods-eigen
#' @export
recover_coord.eigen <- function(x) {
  paste0("E", seq_along(recover_inertia(x)))
}

#' @rdname methods-eigen
#' @export
recover_conference.eigen <- function(x) {
  # `base::eigen()` returns the matrix of eigenvectors
  c(0, 0)
}

#' @rdname methods-eigen
#' @export
augment_u.eigen <- function(x) {
  .name <- rownames(attr(x, "x"))
  res <- if (is.null(.name)) {
    tibble_pole(nrow(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-eigen
#' @export
augment_v.eigen <- function(x) {
  .name <- colnames(attr(x, "x"))
  res <- if (is.null(.name)) {
    tibble_pole(ncol(attr(x, "x")))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-eigen
#' @export
augment_coord.eigen <- function(x) {
  w <- 1:(min(which(c(x$values, -1) < 0)) - 1)
  tibble(
    .name = recover_coord(x),
    .values = x$values[w]
  )
}

#' @rdname methods-eigen
#' @export
negate_to.eigen <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_tbl_ord(x), .matrix), y)
  # tag 'eigen' object with negation
  x <- attribute_alignment(x, diag(s, nrow = min(which(c(x$values, -1) < 0)) - 1))
  # return annotated object
  x
}
