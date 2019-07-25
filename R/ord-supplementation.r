#' @title Supplement ordination objects with new data
#'
#' @description These functions attach supplementary rows or columns to an
#'   ordination object.
#'   

#' @details
#'
#' The `supplementation_*()` methods produce matrices of supplemental rows or
#' columns of a `tbl_ord` object from the object itself. The motivating example
#' is linear discriminant analysis, which produces a natural biplot of class
#' discriminant centroids and variable axes but is usually supplemented with
#' case discriminant scores.
#'
#' The `supplement_*()` functions return the ordination object with either or
#' both factors attributed with the result of `supplementation_*()`. The
#' supplementary values are augmented with a `.supplement` column whose value
#' indicates their source and can be incorporated into a [tidied form][fortify].

#' @name supplementation
#' @include ord-augmentation.r
#' @inheritParams accessors
#' @param supp A matrix with columns `get_coord(x)` of supplementary rows or
#'   columns, i.e. rows of \eqn{U} or of \eqn{V}.
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

supplement_factor <- function(x, .matrix) {
  sup <- supplementation_factor(x, .matrix)
  if (is.null(sup)) return(x)
  supp <- as.data.frame(do.call(rbind, sup))
  supp$.matrix <- .matrix
  supp$.supplement <- rep(names(sup), sapply(sup, nrow))
  set_supplementation_factor(x, supp, .matrix = .matrix)
}

#' @rdname supplementation
#' @export
supplement_u <- function(x) supplement_factor(x, .matrix = "u")

#' @rdname supplementation
#' @export
supplement_v <- function(x) supplement_factor(x, .matrix = "v")

#' @rdname supplementation
#' @export
set_supplementation_u <- function(x, supp) {
  if (! is.null(supp)) {
    stopifnot(is.data.frame(supp))
    print(colnames(supp))
    print(dim(x))
    stopifnot(all(colnames(supp)[1:dim(x)] == get_coord(x)))
  }
  attr(x, "u_supplement") <- supp
  x
}

#' @rdname supplementation
#' @export
set_supplementation_v <- function(x, supp) {
  if (! is.null(supp)) {
    stopifnot(is.data.frame(supp))
    stopifnot(all(colnames(supp) == get_coord(x)))
  }
  attr(x, "v_supplement") <- supp
  x
}

set_supplementation_factor <- function(x, supp, .matrix) {
  switch(
    match_factor(.matrix),
    u = set_supplementation_u(x, supp),
    v = set_supplementation_v(x, supp)
  )
}
