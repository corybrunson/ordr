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
#'         bibble using \strong{\link[dplyr]{dplyr}} verbs adapted to bibbles.
#'         \emph{These are not yet implemented.}
#' }
#' Once the \strong{dplyr} verbs are implemented, each \code{augment_*} function
#' will retrieve both sources and combines them using 
#' \code{link[dplyr]{bind_cols}}.

#' @name bibble-augmentation
#' @include bibble-factors.r
#' @inheritParams bibble-factors

#' @rdname bibble-augmentation
#' @export
augment_u <- function(x) UseMethod("augment_u")

#' @rdname bibble-augmentation
#' @export
augment_v <- function(x) UseMethod("augment_v")

#' @rdname bibble-augmentation
#' @export
augment_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = augment_u(x),
    v = augment_v(x),
    uv = list(u = augment_u(x), v = augment_v(x))
  )
}

#' @rdname bibble-augmentation
#' @export
augment_coord <- function(x) UseMethod("augment_coord")
