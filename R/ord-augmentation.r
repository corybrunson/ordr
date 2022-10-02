#' @title Augment factors and coordinates of 'tbl_ord' objects
#'
#' @description These functions return data associated with the cases,
#'   variables, and coordinates of an ordination object, and attach it to the
#'   object.
#'   

#' @details
#'
#' The `recover_aug_*()` [S3 methods][base::S3Methods] produce
#' [tibble][tibble::tibble]s of values associated with the rows, columns, and
#' artificial coordinates of an object of class '[tbl_ord]'. The first field of
#' each tibble is `name`, which contains the row, column, or coordinate names.
#' Additional fields contain information about the rows, columns, or coordinates
#' extracted from the ordination object.
#'
#' The function `augment_ord()` returns the ordination with either or both
#' matrix factors annotated with the result of `recover_aug_*()`. In this way
#' `augment_ord()` works like [generics::augment()], as popularized by the
#' **broom** package, by extracting information about the rows and columns, but
#' it differs in returning an annotated 'tbl_ord' rather than a
#' ['tbl_df'][tibble::tbl_df] object. The advantage of implementing separate
#' methods for the rows, columns, and artificial coordinates is that more
#' information contained in the original object becomes accessible to the user.
#' 

#' @name augmentation
#' @include ord-recoverers.r
#' @inheritParams recoverers
#' @template param-matrix
#' @return The `recover_aug_*()` functions return [tibble][tibble::tibble]s
#'   having the same numbers of rows as `recover_*()`. `augment_ord()` returns
#'   an augmented tbl_ord with the wrapped model unchanged.
#' @family generic recoverers
#' @seealso [tidiers] and [annotation] methods that interface with augmentation.
NULL

#' @rdname augmentation
#' @export
recover_aug_rows <- function(x) UseMethod("recover_aug_rows")

#' @rdname augmentation
#' @export
recover_aug_cols <- function(x) UseMethod("recover_aug_cols")

recover_aug_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = recover_aug_rows(x),
    cols = recover_aug_cols(x),
    dims = list(rows = recover_aug_rows(x), cols = recover_aug_cols(x))
  )
}

#' @rdname augmentation
#' @export
recover_aug_coord <- function(x) UseMethod("recover_aug_coord")

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
  aug <- recover_aug_factor(x, .matrix)
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
