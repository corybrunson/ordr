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
  
}

#' @rdname methods-cancor
#' @export
recover_cols.cmds_ord <- function(x) recover_dims_cmds(x, "cols")

#' @rdname methods-cancor
#' @export
recover_inertia.cmds_ord <- function(x) x$eig[seq(ncol(x$points))] ^ 2

#' @rdname methods-cancor
#' @export
recover_coord.cmds_ord <- function(x) paste0("PCo", 1:ncol(x$points))

#' @rdname methods-cancor
#' @export
recover_conference.cmds_ord <- function(x) {
  # `stats::cmdscale()` returns the approximate square root
  c(.5, .5)
}

#' @rdname methods-cancor
#' @export
augmentation_rows.cmds_ord <- function(x) {
  .name <- rownames(x$points)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cancor
#' @export
augmentation_cols.cmds_ord <- function(x) {
  .name <- rownames(x$points)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cancor
#' @export
augmentation_coord.cmds_ord <- function(x) {
  tibble(
    .name = factor_coord(recover_coord(x)),
    .eig = x$eig[1:ncol(x$points)]
  )
}
