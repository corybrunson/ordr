#' Functionality for CUR decompositions
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"CURobj"} (see \code{\link[rCUR]{`CURobj-class`}}).
#' 
#' @name methods-rcur
#' @template methods-params
#' @template matrix-param
#' @example inst/examples/ex-rcur.r

#' @rdname methods-rcur
#' @export
as_tbl_ord.CURobj <- as_tbl_ord_default

#' @rdname methods-rcur
#' @export
recover_u.CURobj <- function(x) attr(x, "C")

#' @rdname methods-rcur
#' @export
recover_v.CURobj <- function(x) attr(x, "R")

#' @rdname methods-rcur
#' @export
recover_inertia.CURobj <- function(x) attr(x, "U")

#' @rdname methods-rcur
#' @export
recover_coord.CURobj <- NULL

#' @rdname methods-rcur
#' @export
augment_u.CURobj <- function(x) {
  tibble(
    .name = rownames(attr(x, "C")),
    .leverage.score = attr(x, "R.leverage.score")
  )
}

#' @rdname methods-rcur
#' @export
augment_v.CURobj <- function(x) {
  tibble(
    .name = colnames(attr(x, "R")),
    .leverage.score = attr(x, "C.leverage.score")
  )
}

#' @rdname methods-rcur
#' @export
augment_coord.CURobj <- NULL
