#' @title Functionality for eigendecompositions
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"eigen_ord"` returned by [eigen_ord()].
#'
#' @name methods-eigen
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/karate-igraph-eigen.r
NULL

#' @rdname methods-eigen
#' @export
as_tbl_ord.eigen_ord <- as_tbl_ord_default

recover_uv_eigen <- function(x, .matrix) x[["vectors"]]

#' @rdname methods-eigen
#' @export
recover_u.eigen_ord <- function(x) recover_uv_eigen(x, "u")

#' @rdname methods-eigen
#' @export
recover_v.eigen_ord <- function(x) recover_uv_eigen(x, "v")

#' @rdname methods-eigen
#' @export
recover_inertia.eigen_ord <- function(x) x[["values"]]

#' @rdname methods-eigen
#' @export
recover_coord.eigen_ord <- function(x) colnames(x[["vectors"]])

#' @rdname methods-eigen
#' @export
recover_conference.eigen_ord <- function(x) {
  # `eigen()` returns the matrix of eigenvectors
  c(0, 0)
}

#' @rdname methods-eigen
#' @export
augmentation_u.eigen_ord <- function(x) {
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
augmentation_v.eigen_ord <- function(x) {
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
augmentation_coord.eigen_ord <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .values = x[["values"]]
  )
}
