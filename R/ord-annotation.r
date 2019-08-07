#' @title Annotate the factors of a tbl_ord
#'
#' @description These functions annotate the factors \eqn{U} and \eqn{V} of
#'   [tbl_ord] objects with additional variables, and retrieve these
#'   annotations.
#'   

#' The `annotation_*()` and `set_annotation_*()` functions assign and retrieve
#' values of the `"*_annotation"` attributes of `x`, which should have the same
#' number of rows as `get_*(x)`.
#' 

#' @name annotation
#' @include ord-augmentation.r
#' @inheritParams accessors
#' @param annot A [data.frame][base::data.frame] having the same number of rows
#'   as `get_*(x)`.
NULL

#' @rdname annotation
#' @export
set_annotation_u <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), ".matrix")
  attr(x, "u_annotation") <- annot[, ! (names(annot) %in% protect_vars)]
  x
}

#' @rdname annotation
#' @export
set_annotation_v <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), ".matrix")
  attr(x, "v_annotation") <- annot[, ! (names(annot) %in% protect_vars)]
  x
}

set_annotation_factor <- function(x, annot, .matrix) {
  switch(
    match_factor(.matrix),
    u = set_annotation_u(x, annot),
    v = set_annotation_v(x, annot)
  )
}

#' @rdname annotation
#' @export
annotation_u <- function(x) {
  if (is.null(attr(x, "u_annotation"))) {
    tibble_pole(nrow(get_u(x)))
  } else {
    attr(x, "u_annotation")
  }
}

#' @rdname annotation
#' @export
annotation_v <- function(x) {
  if (is.null(attr(x, "v_annotation"))) {
    tibble_pole(nrow(get_v(x)))
  } else {
    attr(x, "v_annotation")
  }
}

annotation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = annotation_u(x),
    v = annotation_v(x),
    uv = list(u = annotation_u(x), v = annotation_v(x))
  )
}
