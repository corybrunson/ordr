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

recover_dims_cmds <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x$points
  dimnames(res) <- list(
    dimnames(x$x)[[switch(.matrix, rows = 1L, cols = 2L)]],
    recover_coord(x)
  )
  res
}

#' @rdname methods-cmds
#' @export
recover_rows.cmds_ord <- function(x) recover_dims_cmds(x, "rows")

#' @rdname methods-cmds
#' @export
recover_cols.cmds_ord <- function(x) recover_dims_cmds(x, "cols")

#' @rdname methods-cmds
#' @export
recover_inertia.cmds_ord <- function(x) x$eig[seq(ncol(x$points))] ^ 2

#' @rdname methods-cmds
#' @export
recover_coord.cmds_ord <- function(x) paste0("PCo", 1:ncol(x$points))

#' @rdname methods-cmds
#' @export
recover_conference.cmds_ord <- function(x) {
  # `stats::cmdscale()` returns the approximate square root
  c(.5, .5)
}

#' @rdname methods-cmds
#' @export
recover_aug_rows.cmds_ord <- function(x) {
  name <- rownames(x$points)
  res <- if (is.null(name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(name = name)
  }
  res
}

#' @rdname methods-cmds
#' @export
recover_aug_cols.cmds_ord <- function(x) {
  name <- rownames(x$points)
  res <- if (is.null(name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(name = name)
  }
  res
}

#' @rdname methods-cmds
#' @export
recover_aug_coord.cmds_ord <- function(x) {
  tibble(
    name = factor_coord(recover_coord(x)),
    eig = x$eig[1:ncol(x$points)]
  )
}
