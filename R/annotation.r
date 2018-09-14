#' Annotate the factors of an ordination object
#'
#' These functions annotate the factors \eqn{U} and \eqn{V} of \code{tbl_ord}
#' objects with additional variables, and retrieve these annotations.
#' 

#' The \code{annotation_*} and \code{set_annotation_*} functions assign and
#' retrieve values of the \code{"*_annotation"} attributes of \code{x}, which
#' should have the same number of rows as \code{get_*(x)}. \code{annotate_*(x)}
#' returns \code{get_*(x)}, annotated with \code{attr(x, "*_annotation")}.

#' @name annotation
#' @include augmentation.r
#' @inheritParams accessors
#' @param annot A \link{data.frame} having the same number of rows as
#'   \code{get_*(x)}.

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

#' @rdname annotation
#' @export
annotate_u <- function(x) bind_cols(
  as_tibble(get_u(x, align = TRUE)),
  attr(x, "u_annotation")
)

#' @rdname annotation
#' @export
annotate_v <- function(x) bind_cols(
  as_tibble(get_v(x, align = TRUE)),
  attr(x, "v_annotation")
)
