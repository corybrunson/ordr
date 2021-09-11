#' @title Functionality for canonical correlations
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"cancor_ord"`. This is a class introduced in this package
#'   to identify objects returned by [cancor_ord()], which wraps
#'   [stats::cancor()].
#'
#' @name methods-cancor
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/ex-methods-cancor-savings.r
NULL

#' @rdname methods-cancor
#' @export
as_tbl_ord.cancor_ord <- as_tbl_ord_default

#' @rdname methods-cancor
#' @export
recover_rows.cancor_ord <- function(x) {
  res <- x$xcoef[, seq_along(x$cor)]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_cols.cancor_ord <- function(x) {
  res <- x$ycoef[, seq_along(x$cor)]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cancor
#' @export
recover_inertia.cancor_ord <- function(x) x$cor^2

#' @rdname methods-cancor
#' @export
recover_coord.cancor_ord <- function(x) paste0("CanCor", seq_along(x$cor))

#' @rdname methods-cancor
#' @export
recover_conference.cancor_ord <- function(x) {
  # `stats::cancor()` returns canonical weights, i.e. standard coefficients
  c(0, 0)
}

#' @rdname methods-cancor
#' @export
augmentation_rows.cancor_ord <- function(x) {
  .name <- rownames(x$xcoef)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$xcoef))
  } else {
    tibble(.name = .name)
  }
  res$.center <- unname(x$xcenter)
  res
}

#' @rdname methods-cancor
#' @export
augmentation_cols.cancor_ord <- function(x) {
  .name <- rownames(x$ycoef)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$ycoef))
  } else {
    tibble(.name = .name)
  }
  res$.center <- unname(x$ycenter)
  res
}

#' @rdname methods-cancor
#' @export
augmentation_coord.cancor_ord <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .cor = x$cor
  )
}
