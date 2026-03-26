#' @title Functionality for classical multidimensional scaling objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"cmds_ord"`. This is a class introduced in this package
#'   to identify objects returned by [cmdscale_ord()], which wraps
#'   [stats::cmdscale()].
#'
#' @name methods-cmds
#' @include ord-tbl.r
#' @template param-methods
#' @template return-methods
#' @family methods for eigen-decomposition-based techniques
#' @family models from the stats package
#' @example inst/examples/ex-methods-cmds-cities.r
NULL

#' @rdname methods-cmds
#' @export
as_tbl_ord.cmds_ord <- as_tbl_ord_default

#' @rdname methods-cmds
#' @export
recover_rows.cmds_ord <- function(x) {
  res <- x[["points"]]
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-cmds
#' @export
recover_cols.cmds_ord <- function(x) {
  matrix(
    nrow = 0L, ncol = ncol(x[["points"]]),
    dimnames = list(NULL, recover_coord(x))
  )
}

#' @rdname methods-cmds
#' @export
recover_inertia.cmds_ord <- function(x) x$eig[seq(ncol(x$points))] ^ 2

#' @rdname methods-cmds
#' @export
recover_coord.cmds_ord <- function(x) paste0("PCo", seq(ncol(x$points)))

#' @rdname methods-cmds
#' @export
recover_conference.cmds_ord <- function(x) {
  # `stats::cmdscale()` returns the approximate square root
  # `points = evec * sqrt(ev)`, so row elements have full inertia
  c(1, 0)
}

#' @rdname methods-cmds
#' @export
recover_aug_rows.cmds_ord <- function(x) {
  name <- rownames(x$points)
  res <- if (is.null(name)) {
    tibble(.rows = nrow(x$x))
  } else {
    tibble(name = name)
  }
  res$.element <- "active"
  res
}

#' @rdname methods-cmds
#' @export
recover_aug_cols.cmds_ord <- function(x) {
  res <- tibble(.rows = 0L)
  res$.element <- "active"
  res
}

#' @rdname methods-cmds
#' @export
recover_aug_coord.cmds_ord <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    eig = x$eig[seq(ncol(x$points))]
  )
}
