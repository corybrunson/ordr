#' @title Functionality for log-ratio analysis ('lra') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"lra"`, a class introduced in this package to organize
#'   the singular value decomposition of a double-centered log-transformed data
#'   matrix output by [lra()].
#'
#' @name methods-lra
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for singular value decomposition-based techniques
#' @example inst/examples/ex-methods-lra-arrests.r
NULL

#' @rdname methods-lra
#' @export
as_tbl_ord.lra <- as_tbl_ord_default

#' @rdname methods-lra
#' @export
recover_rows.lra <- function(x) {
  x[["row.coords"]]
}

#' @rdname methods-lra
#' @export
recover_cols.lra <- function(x) {
  x[["column.coords"]]
}

#' @rdname methods-lra
#' @export
recover_inertia.lra <- function(x) x[["sv"]] ^ 2

#' @rdname methods-lra
#' @export
recover_coord.lra <- function(x) {
  colnames(x[["row.coords"]])
}

#' @rdname methods-lra
#' @export
recover_conference.lra <- function(x) {
  c(0, 0)
}

#' @rdname methods-lra
#' @export
recover_aug_rows.lra <- function(x) {
  name <- rownames(x[["row.coords"]])
  res <- if (is.null(name)) {
    tibble_pole(nrow(x[["row.coords"]]))
  } else {
    tibble(name = name)
  }
  res$weight <- x$row.weights
  res
}

#' @rdname methods-lra
#' @export
recover_aug_cols.lra <- function(x) {
  name <- rownames(x[["column.coords"]])
  res <- if (is.null(name)) {
    tibble_pole(nrow(x[["column.coords"]]))
  } else {
    tibble(name = name)
  }
  res$weight <- x$column.weights
  res
}

#' @rdname methods-lra
#' @export
recover_aug_coord.lra <- function(x) {
  tibble(name = factor_coord(recover_coord(x)))
}
