#' @title Functionality for eigen-decompositions
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"eigen"` returned by [base::eigen()] when the parameter
#'   `only.values` is set to `FALSE` or of class `"eigen_ord"` returned by
#'   [eigen_ord()].
#'
#' @details
#'
#' [base::eigen()] usually returns an object of class `"eigen"`, which contains
#' the numerical eigendecomposition without annotations such as row and column
#' names. To facilitate downstream analysis, [eigen_ord()] returns a modified
#' 'eigen' object with row names taken (if available) from the original data and
#' column names indicating the integer index of each eigenvector.
#'
#' @name methods-eigen
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for eigen-decomposition-based techniques
#' @family models from the base package
#' @example inst/examples/ex-methods-eigen.r
NULL

#' @rdname methods-eigen
#' @export
as_tbl_ord.eigen <- as_tbl_ord_default

recover_dims_eigen <- function(x, .matrix) x[["vectors"]]

#' @rdname methods-eigen
#' @export
recover_rows.eigen <- function(x) {
  `colnames<-`(recover_dims_eigen(x, "rows"), recover_coord(x))
}

#' @rdname methods-eigen
#' @export
recover_cols.eigen <- function(x) {
  `colnames<-`(recover_dims_eigen(x, "cols"), recover_coord(x))
}

#' @rdname methods-eigen
#' @export
recover_inertia.eigen <- function(x) x[["values"]]

#' @rdname methods-eigen
#' @export
recover_coord.eigen <- function(x) paste0("EV", seq_along(x[["values"]]))

#' @rdname methods-eigen
#' @export
recover_conference.eigen <- function(x) {
  # `eigen()` returns the matrix of eigenvectors
  c(0, 0)
}

#' @rdname methods-eigen
#' @export
recover_aug_rows.eigen_ord <- function(x) tibble(.rows = nrow(x[["vectors"]]))

#' @rdname methods-eigen
#' @export
recover_aug_cols.eigen_ord <- function(x) tibble(.rows = nrow(x[["vectors"]]))

#' @rdname methods-eigen
#' @export
recover_aug_coord.eigen <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    values = x[["values"]]
  )
}

#' @rdname methods-eigen
#' @export
as_tbl_ord.eigen_ord <- as_tbl_ord_default

#' @rdname methods-eigen
#' @export
recover_rows.eigen_ord <- recover_rows.eigen

#' @rdname methods-eigen
#' @export
recover_cols.eigen_ord <- recover_cols.eigen

#' @rdname methods-eigen
#' @export
recover_inertia.eigen_ord <- recover_inertia.eigen

#' @rdname methods-eigen
#' @export
recover_coord.eigen_ord <- recover_coord.eigen

#' @rdname methods-eigen
#' @export
recover_conference.eigen_ord <- recover_conference.eigen

#' @rdname methods-eigen
#' @export
recover_aug_rows.eigen_ord <- function(x) {
  name <- rownames(x[["vectors"]])
  res <- if (is.null(name)) {
    tibble(.rows = nrow(x[["vectors"]]))
  } else {
    tibble(name = name)
  }
  res
}

#' @rdname methods-eigen
#' @export
recover_aug_cols.eigen_ord <- function(x) {
  name <- rownames(x[["vectors"]])
  res <- if (is.null(name)) {
    tibble(.rows = nrow(x[["vectors"]]))
  } else {
    tibble(name = name)
  }
  res
}

#' @rdname methods-eigen
#' @export
recover_aug_coord.eigen_ord <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    values = x[["values"]]
  )
}
