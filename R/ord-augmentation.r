#' @title Augment factors and coordinates of 'tbl_ord's
#'
#' @description These functions return data associated with the cases,
#'   variables, and coordinates of an ordination object, and attach it to the
#'   object.
#'   

#' @details
#'
#' The `augmentation_*()` methods produce [tibble][tibble::tibble]s of values
#' associated with the rows, columns, and artificial coordinates of a
#' '[tbl_ord]' object. The first field of each tibble is `.name`, which contains
#' the row, column, or coordinate names. Additional fields contain information
#' about the rows, columns, or coordinates extracted from the ordination object.
#'
#' The function `augment_ord()` returns the ordination with either or both
#' matrix factors annotated with the result of `augmentation_*()`. In this way
#' `augment_ord()` works like [generics::augment()], as popularized by the
#' **broom** package, by extracting information about the rows and columns, but
#' it differs in returning an annotated 'tbl_ord' rather than a
#' ['tbl_df'][tibble::tbl_df]. The advantage of implementing separate methods
#' for the rows, columns, and artificial coordinates is that more information
#' contained in the original object becomes accessible to the user.
#' 

#' @name augmentation
#' @include ord-accessors.r
#' @inheritParams accessors
#' @param x A '[tbl_ord]' object.
#' @template param-matrix
#' @seealso [tidiers], [annotation]
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
augmentation_coord <- function(x) UseMethod("augmentation_coord")

#' @rdname augmentation
#' @export
augment_ord <- function(x, .matrix = "dims") {
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
