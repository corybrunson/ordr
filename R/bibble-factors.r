#' Extract the factors of an ordination object
#' 
#' These functions return information about the matrix factorization underlying
#' an ordination.
#' 

#' The \code{recover_*} functions extract one or both of the matrix factors 
#' \eqn{U,V} that constitute the original ordination. These are interpreted as 
#' the case scores (\eqn{U}) and the variable loadings (\eqn{V}). The 
#' \code{get_*} functions optionally (and by default) apply any alignment stored
#' as the \code{"align"} attribute (see \code{\link{bibble-alignment}}). Only
#' the \code{recover_*} functions are generics that require methods for each
#' ordination class.
#' 
#' \code{get_coord} retrieves the names of the coordinates shared by \eqn{U} and
#' \eqn{V}, on which the original data were ordinated, and \code{dim.bbl} 
#' retrieves their number.
#' 

#' @name bibble-factors
#' @include bibble-utils.r
#' @param x A bibble, or an ordination object coercible to one.
#' @param ... Additional arguments from \code{as.matrix}; ignored.
#' @template matrix-param
#' @param align Logical; whether to align the matrix factors and coordinates
#'   according to an \code{"align"} matrix attribute.

#' @rdname bibble-factors
#' @export
recover_u <- function(x) UseMethod("recover_u")

#' @rdname bibble-factors
#' @export
recover_v <- function(x) UseMethod("recover_v")

#' @rdname bibble-factors
#' @export
recover_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = recover_u(x),
    v = recover_v(x),
    uv = list(u = recover_u(x), v = recover_v(x))
  )
}

# need `recover_*` functions before and after coercion;
# `recover_*.bbl` are unnecessary

#' @rdname bibble-factors
#' @export
recover_u.default <- function(x) x$u

#' @rdname bibble-factors
#' @export
recover_v.default <- function(x) x$v

# for fortified bibbles (also coordinates?)

#' @rdname bibble-factors
#' @export
recover_u.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 1, -match(".matrix", names(x))]
}

#' @rdname bibble-factors
#' @export
recover_v.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 2, -match(".matrix", names(x))]
}

#' @rdname bibble-factors
#' @export
get_u <- function(x, align = TRUE) {
  u <- recover_u(x)
  if (align && !is.null(attr(x, "align"))) {
    r <- attr(x, "align")
    return(u %*% r)
  } else {
    return(u)
  }
}

#' @rdname bibble-factors
#' @export
get_v <- function(x, align = TRUE) {
  v <- recover_v(x)
  if (align && !is.null(attr(x, "align"))) {
    r <- attr(x, "align")
    return(v %*% r)
  } else {
    return(v)
  }
}

#' @rdname bibble-factors
#' @export
get_factor <- function(x, .matrix, align = TRUE) {
  switch(
    match_factor(.matrix),
    u = get_u(x, align = align),
    v = get_v(x, align = align),
    uv = list(u = get_u(x, align = align), v = get_v(x, align = align))
  )
}

#' @rdname bibble-factors
#' @export
as.matrix.bbl <- function(x, ..., .matrix, align = TRUE) {
  get_factor(x, .matrix = .matrix, align = align)
}

#' @rdname bibble-factors
#' @export
recover_coord <- function(x) UseMethod("recover_coord")

#' @rdname bibble-factors
#' @export
recover_coord.default <- function(x) {
  intersect(colnames(recover_u(x)), colnames(recover_v(x)))
}

#' @rdname bibble-factors
#' @export
get_coord <- function(x, align = TRUE) {
  if (!align || is.null(attr(x, "align"))) {
    recover_coord(x)
  } else {
    colnames(as.data.frame(matrix(1:dim(x), nrow = 1)))
  }
}

#' @rdname bibble-factors
#' @export
dim.bbl <- function(x) length(recover_coord(x))
