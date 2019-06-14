#' @title Access factors, coordinates, and metadata from ordination objects
#'
#' @description These functions return information about the matrix
#'   factorization underlying an ordination.
#'   

#' @details
#'
#' The `recover_*()` functions extract one or both of the matrix factors
#' \eqn{U,V} that constitute the original ordination. These are interpreted as
#' the case scores (\eqn{U}) and the variable loadings (\eqn{V}). The `get_*()`
#' functions optionally (and by default) apply any alignment stored as the
#' `"align"` attribute (see [alignment]). Only the `recover_*()` functions are
#' generics that require methods for each ordination class.
#'
#' `get_coord()` retrieves the names of the coordinates shared by \eqn{U} and
#' \eqn{V}, on which the original data were ordinated, and `dim.tbl_ord()`
#' retrieves their number, the rank of the ordination. The outer dimensions of
#' the matrix decomposition are returned by `dim_u()` and `dim_v()`.
#' 

#' @name accessors
#' @include utils.r
#' @param x A `[tbl_ord]`, or an ordination object coercible to one.
#' @param ... Additional arguments from [base::as.matrix()]; ignored.
#' @template param-matrix
#' @param align Logical; whether to align the matrix factors and coordinates
#'   according to an `"align"` matrix attribute.
NULL

#' @rdname accessors
#' @export
recover_u <- function(x) UseMethod("recover_u")

#' @rdname accessors
#' @export
recover_v <- function(x) UseMethod("recover_v")

#' @rdname accessors
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
# `recover_*.tbl_ord` are unnecessary

#' @rdname accessors
#' @export
recover_u.default <- function(x) x$u

#' @rdname accessors
#' @export
recover_v.default <- function(x) x$v

# for fortified tbl_ords (also coordinates?)

#' @rdname accessors
#' @export
recover_u.data.frame <- function(x) {
  x[x$.matrix == "u", -match(".matrix", names(x))]
}

#' @rdname accessors
#' @export
recover_v.data.frame <- function(x) {
  x[x$.matrix == "v", -match(".matrix", names(x))]
}

#' @rdname accessors
#' @export
get_u <- function(x, align = TRUE) {
  u <- recover_u(x)
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    s <- diag(sqrt(recover_inertia(x)) ^ p[1])
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2)
    u <- u %*% s
  }
  if (align && ! is.null(attr(x, "align"))) {
    r <- attr(x, "align")
    return(u %*% r)
  } else {
    return(u)
  }
}

#' @rdname accessors
#' @export
get_v <- function(x, align = TRUE) {
  v <- recover_v(x)
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    s <- diag(sqrt(recover_inertia(x)) ^ p[2])
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2)
    v <- v %*% s
  }
  if (align && ! is.null(attr(x, "align"))) {
    r <- attr(x, "align")
    return(v %*% r)
  } else {
    return(v)
  }
}

#' @rdname accessors
#' @export
get_factor <- function(x, .matrix, align = TRUE) {
  switch(
    match_factor(.matrix),
    u = get_u(x, align = align),
    v = get_v(x, align = align),
    uv = list(u = get_u(x, align = align), v = get_v(x, align = align))
  )
}

#' @rdname accessors
#' @export
as.matrix.tbl_ord <- function(x, ..., .matrix, align = TRUE) {
  get_factor(x, .matrix = .matrix, align = align)
}

#' @rdname accessors
#' @export
recover_inertia <- function(x) UseMethod("recover_inertia")

#' @rdname accessors
#' @export
recover_inertia.default <- function(x) NA_real_

#' @rdname accessors
#' @export
recover_coord <- function(x) UseMethod("recover_coord")

#' @rdname accessors
#' @export
recover_coord.default <- function(x) {
  intersect(colnames(recover_u(x)), colnames(recover_v(x)))
}

#' @rdname accessors
#' @export
recover_coord.data.frame <- function(x) {
  if (! is.null(attr(x, "coordinates"))) {
    attr(x, "coordinates")
  } else {
    recover_coord.default(x)
  }
}

#' @rdname accessors
#' @export
get_coord <- function(x, align = TRUE) {
  if (! align || is.null(attr(x, "align"))) {
    recover_coord(x)
  } else if (! is.null(colnames(get_alignment(x)))) {
    colnames(get_alignment(x))
  } else {
    colnames(as.data.frame(matrix(1:dim(x), nrow = 1)))
  }
}

#' @rdname accessors
#' @export
dim.tbl_ord <- function(x) length(recover_coord(x))

#' @rdname accessors
#' @export
dim_u <- function(x) nrow(recover_u(x))

#' @rdname accessors
#' @export
dim_v <- function(x) nrow(recover_v(x))

#' @rdname accessors
#' @export
dim_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = dim_u(x),
    v = dim_v(x),
    uv = c(u = dim_u(x), v = dim_v(x))
  )
}
