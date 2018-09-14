#' Augment information for the factors and coordinates of ordination objects
#' 
#' These functions return data associated with the cases, variables, and
#' coordinates of an ordination object.
#' 

#' The \code{augmentation_*} functions produce \link[tibble]{tibble}s of values
#' associated with the cases, variables, and coordinates of a \code{tbl_ord}
#' object. The first field of each tibble is \code{.name}, which contains the
#' case, variable, or coordinate names. Additional fields contain information
#' about the cases, variables, or coordinates extracted from the original
#' ordination object.
#'
#' The \code{augment_*} functions return the ordination coordinates data
#' augmented with the result of \code{augmentation_*}. In this sense
#' \code{augment_*} works like \code{\link[broom]{augment}} in extracting all
#' information that can be included in a tidy summary of the components. The
#' advantage of implementing separate methods for the different components is
#' that more information contained in the original object becomes accessible to
#' the user.

#' @name augmentation
#' @include accessors.r
#' @inheritParams accessors

#' @rdname augmentation
#' @export
augmentation_u <- function(x) UseMethod("augmentation_u")

#' @rdname augmentation
#' @export
augmentation_v <- function(x) UseMethod("augmentation_v")

#' @rdname augmentation
#' @export
augmentation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = augmentation_u(x),
    v = augmentation_v(x),
    uv = list(u = augmentation_u(x), v = augmentation_v(x))
  )
}

#' @rdname augmentation
#' @export
augmentation.tbl_ord <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = augmentation_u(x),
    v = augmentation_v(x),
    uv = dplyr::bind_rows(
      dplyr::mutate(augmentation_u(x), .matrix = "u"),
      dplyr::mutate(augmentation_v(x), .matrix = "v")
    )
  )
}

#' @rdname augmentation
#' @export
augmentation_coord <- function(x) UseMethod("augmentation_coord")

#' @rdname augmentation
#' @export
augment_u <- function(x) bind_cols(
  as_tibble(get_u(x, align = TRUE)),
  augmentation_u(x)
)

#' @rdname augmentation
#' @export
augment_v <- function(x) bind_cols(
  as_tibble(get_v(x, align = TRUE)),
  augmentation_v(x)
)

#' @rdname augmentation
#' @export
augment_coord <- function(x) augmentation_coord(x)

#' @importFrom generics augment
#' @export
generics::augment

#' @rdname augmentation
#' @export
augment.tbl_ord <- function(x, ...) {
  aug_u <- bind_cols(annotation_u(x), augmentation_u(x))
  x <- set_annotation_u(x, aug_u)
  aug_v <- bind_cols(annotation_v(x), augmentation_v(x))
  x <- set_annotation_v(x, aug_v)
  x
}
