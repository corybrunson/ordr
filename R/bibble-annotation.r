#' Recover annotation for an ordination object
#' 
#' These functions return annotation associated with the subjects, variables,
#' and coordinates of an ordination.
#' 

#' The \code{*_annot} functions produce \link[tibble]{tibble}s of annotations 
#' for the subjects/scores, variables/loadings, and coordinates. The first field
#' of each tibble is \code{.name} and contains the subject, variable, and 
#' coordinate names, respectively.
#' 
#' The \code{*_annot} functions return annotation from two sources:
#' \enumerate{
#'   \item Information about the subjects, variables, or coordinates contained
#'         in the original ordination object. In this sense \code{*_annot} works
#'         like \code{\link[broom]{tidy}} to extract all such information that
#'         can be included in a tidy summary of the components. The advantage of
#'         implementing separate methods for the different components is that
#'         more information contained in the original object becomes accessible
#'         to the user.
#'   \item Additional annotation about the components manually added to the
#'         bibble using adapted \strong{\link[dplyr]{dplyr}} verbs.
#'         \strong{These are not yet implemented.}
#' }
#' Each \code{*_annot} function retrieves both sources and combines them using
#' \code{link[dplyr]{bind_cols}}.

#' @name bibble-annotation
#' @inheritParams bibble-factors

#' @rdname bibble-annotation
#' @export
u_annot <- function(x) UseMethod("u_annot")

#' @rdname bibble-annotation
#' @export
v_annot <- function(x) UseMethod("v_annot")

#' @rdname bibble-annotation
#' @export
factor_annot <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = u_annot(x),
    v = v_annot(x),
    uv = list(u = u_annot(x), v = v_annot(x))
  )
}

#' @rdname bibble-annotation
#' @export
coord_annot <- function(x) UseMethod("coord_annot")
