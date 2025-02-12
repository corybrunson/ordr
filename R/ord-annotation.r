#' @title Annotate factors of 'tbl_ord' objects
#'
#' @description These functions annotate the matrix factors of [tbl_ord]s with
#'   additional variables, and retrieve these annotations.
#'   

#' The unexported `annotation_*()` and `set_annotation_*()` functions assign and
#' retrieve values of the `"*_annotation"` attributes of `x`, which must have
#' the same number of rows as `get_*(x)`.
#' 

#' @name annotation
#' @include ord-augmentation.r
#' @param annot A [data.frame][base::data.frame] having the same number of rows
#'   as `get_*(x)`.
#' @seealso [augmentation] methods that must interface with annotation.
NULL

set_annotation_rows <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), ".matrix")
  attr(x, "rows_annotation") <- annot[, ! (names(annot) %in% protect_vars)]
  x
}

set_annotation_cols <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), ".matrix")
  attr(x, "cols_annotation") <- annot[, ! (names(annot) %in% protect_vars)]
  x
}

set_annotation_factor <- function(x, annot, .matrix) {
  switch(
    match_factor(.matrix),
    rows = set_annotation_rows(x, annot),
    cols = set_annotation_cols(x, annot)
  )
}

annotation_rows <- function(x) {
  if (is.null(attr(x, "rows_annotation"))) {
    tibble(.rows = nrow(get_rows(x)))
  } else {
    attr(x, "rows_annotation")
  }
}

annotation_cols <- function(x) {
  if (is.null(attr(x, "cols_annotation"))) {
    tibble(.rows = nrow(get_cols(x)))
  } else {
    attr(x, "cols_annotation")
  }
}

annotation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = annotation_rows(x),
    cols = annotation_cols(x),
    dims = list(rows = annotation_rows(x), cols = annotation_cols(x))
  )
}
