#' Augment information for the factors and coordinates of an ordination object
#' 
#' These functions return data associated with the cases, variables, and
#' coordinates of an ordination.
#' 

#' The \code{augment_*} functions produce \link[tibble]{tibble}s of data 
#' augmented to the cases, variables, and coordinates. The first field of each
#' tibble is \code{.name} and contains the case, variable, and coordinate names,
#' respectively.
#' 
#' The \code{augment_*} functions return data augmented from two sources:
#' \enumerate{
#'   \item Information about the cases, variables, or coordinates contained
#'         in the original ordination object. In this sense \code{augment_*}
#'         works like \code{\link[broom]{augment}} to extract all such
#'         information that can be included in a tidy summary of the components.
#'         The advantage of implementing separate methods for the different
#'         components is that more information contained in the original object
#'         becomes accessible to the user.
#'   \item Additional information about the components manually added to the
#'         \code{tbl_ord} using adapted \strong{\link[dplyr]{dplyr}} verbs.
#' }
#' Once the \strong{dplyr} verbs are implemented, each \code{augment_*} function
#' will retrieve both sources and combines them using 
#' \code{link[dplyr]{bind_cols}}.

#' @name augmentation
#' @include factors.r
#' @inheritParams factors
#' @param annot A \code{\link[tibble]{tibble}} having the same number of rows as
#'   \code{x}.

#' @rdname augmentation
#' @export
augment_u <- function(x) UseMethod("augment_u")

#' @rdname augmentation
#' @export
augment_v <- function(x) UseMethod("augment_v")

#' @rdname augmentation
#' @export
augment_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = augment_u(x),
    v = augment_v(x),
    uv = list(u = augment_u(x), v = augment_v(x))
  )
}

#' @rdname augmentation
#' @export
augment.tbl_ord <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = augment_u(x),
    v = augment_v(x),
    uv = dplyr::bind_rows(
      dplyr::mutate(augment_u(x), .matrix = "u"),
      dplyr::mutate(augment_v(x), .matrix = "v")
    )
  )
}

#' @rdname augmentation
#' @export
augment_coord <- function(x) UseMethod("augment_coord")

#' @rdname augmentation
#' @export
annotate_u <- function(x) bind_cols(augment_u(x), attr(x, "u_annot"))

#' @rdname augmentation
#' @export
annotate_v <- function(x) bind_cols(augment_v(x), attr(x, "v_annot"))

#' @rdname augmentation
#' @export
u_annot <- function(x) attr(x, "u_annot")

#' @rdname augmentation
#' @export
v_annot <- function(x) attr(x, "v_annot")

factor_annot <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = u_annot(x),
    v = v_annot(x),
    uv = list(u = u_annot(x), v = v_annot(x))
  )
}

#' @rdname augmentation
#' @export
set_u_annot <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), names(augment_u(x)), ".matrix")
  attr(x, "u_annot") <- annot[, !(names(annot) %in% protect_vars)]
  x
}

#' @rdname augmentation
#' @export
set_v_annot <- function(x, annot) {
  stopifnot(is.data.frame(annot))
  protect_vars <- c(get_coord(x), names(augment_u(x)), ".matrix")
  attr(x, "v_annot") <- annot[, !(names(annot) %in% protect_vars)]
  x
}

set_factor_annot <- function(x, annot, .matrix) {
  switch(
    match_factor(.matrix),
    u = set_u_annot(x, annot),
    v = set_v_annot(x, annot)
  )
}
