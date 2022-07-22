#' @title Supplement 'tbl_ord' objects with new data
#'
#' @description These functions attach supplementary rows or columns to an
#'   ordination object.
#'   

#' @details
#'
#' The `supplementation_*()` [S3 methods][base::S3Methods] produce matrices of
#' supplemental rows or columns of a [tbl_ord] object from the object itself.
#' The motivating example is linear discriminant analysis, which produces a
#' natural biplot of class discriminant centroids and variable axes but is
#' usually supplemented with case discriminant scores. The supplementary values
#' are augmented with an `.element` column whose value indicates their source
#' and can be incorporated into a [tidied form][fortify]. If no supplementary
#' rows of a factor are produced, the functions return `NULL`.
#' 

#' @name supplementation
#' @include ord-augmentation.r
#' @inheritParams accessors
#' @family generic accessors
NULL

#' @rdname supplementation
#' @export
supplementation_rows <- function(x) UseMethod("supplementation_rows")

#' @rdname supplementation
#' @export
supplementation_rows.default <- function(x) NULL

#' @rdname supplementation
#' @export
supplementation_cols <- function(x) UseMethod("supplementation_cols")

#' @rdname supplementation
#' @export
supplementation_cols.default <- function(x) NULL

supplementation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = supplementation_rows(x),
    cols = supplementation_cols(x),
    dims = list(rows = supplementation_rows(x), cols = supplementation_cols(x))
  )
}
