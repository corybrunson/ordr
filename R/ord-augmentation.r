#' @title Augment metadata on the factors and coordinates of tbl_ords
#'
#' @description These functions return data associated with the cases,
#'   variables, and coordinates of an ordination object, and attach it to the
#'   object.
#'   

#' @details
#'
#' The `augmentation_*()` methods produce [tibble][tibble::tibble]s of values
#' associated with the rows, columns, and coordinates of a '[tbl_ord]' object.
#' The first field of each tibble is `.name`, which contains the case, variable,
#' or coordinate names. Additional fields contain information about the cases,
#' variables, or coordinates extracted from the original ordination object.
#'
#' The `augment_*()` functions return the ordination with either or both factors
#' annotated with the result of `augmentation_*()`. In this way `augment_*()`
#' works like [generics::augment()] by extracting information for a tidy summary
#' of the components, but it differs in returning an annotated tbl_ord rather
#' than a ['tbl_df'][tibble::tbl_df]. The advantage of implementing separate
#' methods for the different components is that more information contained in
#' the original object becomes accessible to the user.
#'
#' Three generics popularized by the **ggplot2** and **broom** packages make use
#' of these functions:
#' 

#' * The [generics::augment()] method adds information about
#'   the rows and columns while maintaining 'tbl_ord' class structure.
#' * The [generics::tidy()] method summarizes information about
#'   model components, which here are the artificial coordinates
#'   created by ordinations.
#' * The [ggplot2::fortify()] method ([fortify.tbl_ord()]) augments and
#'   collapses row and/or column data into a single tibble.
#' 

#' @name augmentation
#' @include ord-accessors.r
#' @inheritParams accessors
#' @param data Passed to [generics::augment()]; currently ignored.
#' @param ... Additional arguments allowed by generics; ignored.
#' @example inst/examples/bioenv-lm-isolines.r
#' @example inst/examples/benthos-ca-augment-confer.r
#' @example inst/examples/mtcars-kmeans-augment.r
NULL

#' @rdname augmentation
#' @export
augmentation_rows <- function(x) UseMethod("augmentation_rows")

#' @rdname augmentation
#' @export
augmentation_cols <- function(x) UseMethod("augmentation_cols")

#' @rdname augmentation
#' @export
augmentation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = augmentation_rows(x),
    cols = augmentation_cols(x),
    dims = list(rows = augmentation_rows(x), cols = augmentation_cols(x))
  )
}

#' @rdname augmentation
#' @export
augmentation.tbl_ord <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = augmentation_rows(x),
    cols = augmentation_cols(x),
    dims = dplyr::bind_rows(
      dplyr::mutate(augmentation_rows(x), .matrix = "rows"),
      dplyr::mutate(augmentation_cols(x), .matrix = "cols")
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
augment.tbl_ord <- function(x, data, .matrix = "dims", ...) {
  .matrix <- match_factor(.matrix)
  if (.matrix == "dims" || .matrix == "rows") {
    x <- augment_factor(x, "rows")
  }
  if (.matrix == "dims" || .matrix == "cols") {
    x <- augment_factor(x, "cols")
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
    match_diff <- setdiff(match_names, c(match_vals, NA))
    if (length(match_diff) > 0) {
      warning(
        "Removing annotated field(s) '",
        paste(names(ann)[match_diff], collapse = "', '"),
        "' that are reserved for augmentation."
      )
    }
    ann <- ann[, -which(! is.na(match_names)), drop = FALSE]
  }
  # place augmentation variables first, for consistency
  if (ncol(ann) == 0L) {
    aug
  } else if (ncol(aug) == 0L) {
    ann
  } else {
    bind_cols(aug, ann)
  }
}

augment_factor <- function(x, .matrix) {
  ann <- augment_annotation(x, .matrix)
  set_annotation_factor(x, ann, .matrix = .matrix)
}

#' @rdname augmentation
#' @export
augment_rows <- function(x) augment_factor(x, .matrix = "rows")

#' @rdname augmentation
#' @export
augment_cols <- function(x) augment_factor(x, .matrix = "cols")

#' @importFrom generics tidy
#' @export
generics::tidy

#' @rdname augmentation
#' @export
tidy.tbl_ord <- function(x, ...) {
  bind_cols(
    inertia = recover_inertia(x),
    augmentation_coord(x)
  )
}
