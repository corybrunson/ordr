#' @title Functionality for eigendecompositions
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"eigen"` returned by [eigen_ord()].
#'
#' @name methods-eigen
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/karate-igraph-eigen.r
NULL

#' @rdname methods-eigen
#' @export
as_tbl_ord.eigen <- as_tbl_ord_default

#' @rdname methods-eigen
#' @export
reconstruct.eigen <- function(x) {
  x$vectors %*% diag(x$values) %*% t(x$vectors)
}

recover_uv_eigen <- function(x, .matrix) {
  w <- 1:(min(which(c(x$values, -1) < 0)) - 1)
  res <- x[["vectors"]][, w, drop = FALSE] %*%
    diag(sqrt(x[["values"]][w]), nrow = length(w))
  colnames(res) <- recover_coord(x)
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
  colnames(x[["vectors"]])[seq_along(recover_inertia(x))]
}

#' @rdname methods-eigen
#' @export
recover_conference.eigen <- function(x) {
  # `eigen()` returns the matrix of eigenvectors
  c(0, 0)
}

#' @rdname methods-eigen
#' @export
augmentation_u.eigen <- function(x) {
  .name <- rownames(x[["vectors"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["vectors"]]))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-eigen
#' @export
augmentation_v.eigen <- function(x) {
  .name <- rownames(x[["vectors"]])
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x[["vectors"]]))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-eigen
#' @export
augmentation_coord.eigen <- function(x) {
  w <- 1:(min(which(c(x$values, -1) < 0)) - 1)
  tibble(
    .name = recover_coord(x),
    .values = x$values[w]
  )
}
