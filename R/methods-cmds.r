#' @title Functionality for classical multidimensional scaling objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"cmds"`. This is a class introduced in this package to
#'   identify objects returned by [stats::cmdscale()], which is masked by a
#'   wrapper that adds the class attribute.
#'
#' @name methods-cmds
#' @include ord-tbl.r
#' @template param-methods
#' @example inst/examples/country-cmds-lm.r
#' @example inst/examples/country-cmds-prcomp-negate.r
NULL

#' @rdname methods-cmds
#' @export
as_tbl_ord.cmds <- as_tbl_ord_default

#' @rdname methods-cmds
#' @export
reconstruct.cmds <- function(x) {
  -2 * x$points %*% t(x$points)
}

recover_uv_cmds <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x$points
  dimnames(res) <- list(
    dimnames(x$x)[[switch(.matrix, u = 1, v = 2)]],
    recover_coord(x)
  )
  res
}

#' @rdname methods-cmds
#' @export
recover_u.cmds <- function(x) recover_uv_cmds(x, "u")

#' @rdname methods-cmds
#' @export
recover_v.cmds <- function(x) recover_uv_cmds(x, "v")

#' @rdname methods-cmds
#' @export
recover_inertia.cmds <- function(x) x$eig ^ 2

#' @rdname methods-cmds
#' @export
recover_coord.cmds <- function(x) paste0("PCo", 1:ncol(x$points))

#' @rdname methods-cmds
#' @export
recover_conference.cmds <- function(x) {
  # `stats::cmdscale()` returns the approximate square root
  c(.5, .5)
}

#' @rdname methods-cmds
#' @export
augmentation_u.cmds <- function(x) {
  .name <- rownames(x$points)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cmds
#' @export
augmentation_v.cmds <- function(x) {
  .name <- rownames(x$points)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cmds
#' @export
augmentation_coord.cmds <- function(x) {
  tibble(
    .name = recover_coord(x),
    .eig = x$eig[1:ncol(x$points)]
  )
}
