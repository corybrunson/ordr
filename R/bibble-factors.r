#' Extract the factors of an ordination object
#' 
#' These functions return information about the matrix factorization underlying
#' an ordination.
#' 

#' The \code{get_*} functions extract one or both of the matrix factors 
#' \eqn{U,V} that constitute an ordination. These are interpreted as the subject
#' scores (\eqn{U}) and the variable loadings (\eqn{V}). \code{get_coord} 
#' retrieves the names of the coordinates shared by \eqn{U} and \eqn{V}.
#' 
#' \code{dim.bbl} retrieves the number of coordinates on which the data have
#' been ordinated.
#' 

#' @name bibble-factors
#' @param x A bibble, or an object convertible to one.
#' @param ... Additional arguments from \code{as.matrix}; ignored.
#' @template matrix-param

#' @rdname bibble-factors
#' @export
get_u <- function(x) UseMethod("get_u")

#' @rdname bibble-factors
#' @export
get_v <- function(x) UseMethod("get_v")

# need `get_*` functions before and after coercion; `get_*.bbl` are unnecessary

#' @rdname bibble-factors
#' @export
get_u.default <- function(x) x$u

#' @rdname bibble-factors
#' @export
get_v.default <- function(x) x$v

#' @rdname bibble-factors
#' @export
get_coord.default <- function(x) {
  intersect(colnames(get_u(x)), colnames(get_v(x)))
}

# for fortified bibbles (also coordinates?)

#' @rdname bibble-factors
#' @export
get_u.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 1, -match(".matrix", names(x))]
}

#' @rdname bibble-factors
#' @export
get_v.data.frame <- function(x) {
  x$.matrix <- as.numeric(x$.matrix)
  x[x$.matrix == 2, -match(".matrix", names(x))]
}

#' @rdname bibble-factors
#' @export
get_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = get_u(x),
    v = get_v(x),
    uv = list(u = get_u(x), v = get_v(x))
  )
}

#' @rdname bibble-factors
#' @export
as.matrix.bbl <- function(x, ..., .matrix) get_factor(x, .matrix)

#' @rdname bibble-factors
#' @export
get_coord <- function(x) UseMethod("get_coord")

#' @rdname bibble-factors
#' @export
dim.bbl <- function(x) length(get_coord(x))
