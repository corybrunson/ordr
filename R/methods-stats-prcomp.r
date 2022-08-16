#' @title Functionality for principal components analysis ('prcomp') objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"prcomp"` as returned by [stats::prcomp()].
#'
#' @name methods-prcomp
#' @author Emily Paul
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for singular value decomposition-based techniques
#' @example inst/examples/ex-methods-prcomp-iris.r
NULL

#' @rdname methods-prcomp
#' @export
as_tbl_ord.prcomp <- as_tbl_ord_default

#' @rdname methods-prcomp
#' @export
recover_rows.prcomp <- function(x) {
  x[["x"]]
}

#' @rdname methods-prcomp
#' @export
recover_cols.prcomp <- function(x) {
  x[["rotation"]]
}

#' @rdname methods-prcomp
#' @export
recover_inertia.prcomp <- function(x) {
  (x[["sdev"]] ^ 2) * (nrow(x[["x"]]) - 1)
}

#' @rdname methods-prcomp
#' @export
recover_coord.prcomp <- function(x) {
  colnames(x[["rotation"]])
}

#' @rdname methods-prcomp
#' @export
recover_conference.prcomp <- function(x) {
  # `stats::prcomp()` returns the rotated data
  c(1, 0)
}

#' @rdname methods-prcomp
#' @export
recover_aug_rows.prcomp <- function(x) {
  name <- rownames(x[["x"]])
  if (is.null(name)) {
    tibble_pole(nrow(x[["x"]]))
  } else {
    tibble(name = name)
  }
}

#' @rdname methods-prcomp
#' @export
recover_aug_cols.prcomp <- function(x) {
  name <- rownames(x[["rotation"]])
  res <- if (is.null(name)) {
    tibble_pole(nrow(x[["rotation"]]))
  } else {
    tibble(name = name)
  }
  if (inherits(x[["center"]], "numeric")) {
    res <- dplyr::bind_cols(res, center = x[["center"]])
  }
  if (inherits(x[["scale"]], "numeric")) {
    res <- dplyr::bind_cols(res, scale = x[["scale"]])
  }
  res
}

#' @rdname methods-prcomp
#' @export
recover_aug_coord.prcomp <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    sdev = x[["sdev"]]
  )
}
