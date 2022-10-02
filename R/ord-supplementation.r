#' @title Supplement 'tbl_ord' objects with new data
#'
#' @description These functions attach supplementary rows or columns to an
#'   ordination object.
#'   

#' @details
#'
#' The `recover_supp_*()` [S3 methods][base::S3Methods] produce matrices of
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
#' @inheritParams recoverers
#' @return Matrices having the same numbers of columns as returned by
#'   [recover_rows()] and [recover_cols()], or else `NULL`.
#' @family generic recoverers
NULL

#' @rdname supplementation
#' @export
recover_supp_rows <- function(x) UseMethod("recover_supp_rows")

#' @rdname supplementation
#' @export
recover_supp_rows.default <- function(x) NULL

#' @rdname supplementation
#' @export
recover_supp_cols <- function(x) UseMethod("recover_supp_cols")

#' @rdname supplementation
#' @export
recover_supp_cols.default <- function(x) NULL

recover_supp_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = recover_supp_rows(x),
    cols = recover_supp_cols(x),
    dims = list(rows = recover_supp_rows(x), cols = recover_supp_cols(x))
  )
}
