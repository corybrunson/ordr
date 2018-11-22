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
#' The \code{augment_*} functions return the ordination with each or both factor
#' annotated with the result of \code{augmentation_*}. In this way
#' \code{augment_*} works like \code{\link[broom]{augment}} by extracting
#' information for a tidy summary of the components, but it differs in returning
#' an annotated \code{tbl_ord} rather than a \code{tbl_df} object. The advantage
#' of implementing separate methods for the different components is that more
#' information contained in the original object becomes accessible to the user.
#' To achieve a result similar to that of \code{augment}, use
#' \code{\link{fortify}}.

#' @name augmentation
#' @include accessors.r
#' @inheritParams accessors
#' @param data Passed to \code{\link[generics]{augment}}; currently ignored.

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

#' @importFrom generics augment
#' @export
generics::augment

#' @rdname augmentation
#' @export
augment.tbl_ord <- function(x, data, .matrix = "uv", ...) {
  .matrix <- match_factor(.matrix)
  if (grepl("u", .matrix)) {
    x <- augment_factor(x, "u")
  }
  if (grepl("v", .matrix)) {
    x <- augment_factor(x, "v")
  }
  x
}

augment_annotation <- function(x, .matrix) {
  ann <- annotation_factor(x, .matrix)
  aug <- augmentation_factor(x, .matrix)
  # remove any columns of `ann` that overlap wth those of `aug`
  match_vals <- match(ann, aug)
  match_names <- match(names(ann), names(aug))
  #match_vars <- ifelse(match_vals == match_names, match_vals, NA)
  # if any augmentation names are duplicated in the annotation...
  if (any(! is.na(match_names))) {
    # if any duplicated names in the annotation are different from augmentation
    match_diff <- setdiff(match_names, match_vals)
    if (length(match_diff) > 0) {
      warning(
        "Removing annotated field(s) '",
        paste(names(ann)[match_diff], collapse = "', '"),
        "' that are reserved for augmentation."
      )
    }
    ann <- ann[, -which(! is.na(match_names)), drop = FALSE]
  }
  bind_cols(ann, aug)
}

augment_factor <- function(x, .matrix) {
  ann <- augment_annotation(x, .matrix)
  set_annotation_factor(x, ann, .matrix = .matrix)
}

#' @rdname augmentation
#' @export
augment_u <- function(x) augment_factor(x, .matrix = "u")

#' @rdname augmentation
#' @export
augment_v <- function(x) augment_factor(x, .matrix = "v")
