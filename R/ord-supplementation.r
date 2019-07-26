#' @title Supplement ordination objects with new data
#'
#' @description These functions attach supplementary rows or columns to an
#'   ordination object.
#'   

#' @details
#'
#' The `supplementation_*()` methods produce matrices of supplemental rows or
#' columns (i.e. rows of \eqn{U} or of \eqn{V}) of a `tbl_ord` object from the
#' object itself. The motivating example is linear discriminant analysis, which
#' produces a natural biplot of class discriminant centroids and variable axes
#' but is usually supplemented with case discriminant scores. If no
#' supplementary rows of a factor are produced, the function returns `NULL`.
#'
#' The `supplement_*()` functions return the ordination object with either or
#' both factors attributed with the result of `supplementation_*()`. The
#' supplementary values are augmented with a `.supplement` column whose value
#' indicates their source and can be incorporated into a [tidied form][fortify].

#' @name supplementation
#' @include ord-augmentation.r
#' @inheritParams accessors
#' @example inst/examples/diabetes-lda.r
NULL

#' @rdname supplementation
#' @export
supplementation_u <- function(x) UseMethod("supplementation_u")

supplementation_u.default <- function(x) NULL

#' @rdname supplementation
#' @export
supplementation_v <- function(x) UseMethod("supplementation_v")

supplementation_v.default <- function(x) NULL

#' @rdname supplementation
#' @export
supplementation_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = supplementation_u(x),
    v = supplementation_v(x),
    uv = list(u = supplementation_u(x), v = supplementation_v(x))
  )
}
