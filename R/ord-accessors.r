#' @title Access factors, coordinates, and metadata from ordination objects
#'
#' @description These functions return information about the matrix
#'   factorization underlying an ordination.
#'   

#' @details
#'
#' The `recover_*()` functions extract one or both of the row and column matrix
#' factors that constitute the original ordination. These are interpreted as the
#' case scores (rows) and the variable loadings (columns). The `get_*()`
#' functions optionally (and by default) include any supplemental observations
#' (see [supplementation]). Only the `recover_*()` functions are generics that
#' require methods for each ordination class.
#'
#' `get_coord()` retrieves the names of the coordinates shared by the matrix
#' factors on which the original data were ordinated, and `dim()` retrieves
#' their number, the rank of the ordination. The outer dimensions of the matrix
#' decomposition are returned by `dim_rows()` and `dim_cols()`.
#' 

#' @name accessors
#' @include utils.r
#' @param x A '[tbl_ord]' object, or an ordination object coercible to one.
#' @param ... Additional arguments from [base::as.matrix()]; ignored.
#' @template param-matrix
#' @param .supplement Logical; whether to include
NULL

#' @rdname accessors
#' @export
recover_rows <- function(x) UseMethod("recover_rows")

#' @rdname accessors
#' @export
recover_cols <- function(x) UseMethod("recover_cols")

#' @rdname accessors
#' @export
recover_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = recover_rows(x),
    cols = recover_cols(x),
    dims = list(rows = recover_rows(x), cols = recover_cols(x))
  )
}

# need `recover_*` functions before and after coercion;
# `recover_*.tbl_ord` are unnecessary

#' @rdname accessors
#' @export
recover_rows.default <- function(x) x$u

#' @rdname accessors
#' @export
recover_cols.default <- function(x) x$v

# for fortified tbl_ords (also coordinates?)

#' @rdname accessors
#' @export
recover_rows.data.frame <- function(x) {
  x[x$.matrix == "rows", -match(".matrix", names(x))]
}

#' @rdname accessors
#' @export
recover_cols.data.frame <- function(x) {
  x[x$.matrix == "cols", -match(".matrix", names(x))]
}

#' @rdname accessors
#' @export
get_rows <- function(x, .supplement = TRUE) {
  u <- recover_rows(x)
  if (.supplement) u <- rbind(u, supplementation_rows(x))
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    s <- diag(sqrt(recover_inertia(x)) ^ p[1])
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2)
    u <- u %*% s
  }
  return(u)
}

#' @rdname accessors
#' @export
get_cols <- function(x, .supplement = TRUE) {
  v <- recover_cols(x)
  if (.supplement) v <- rbind(v, supplementation_cols(x))
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    s <- diag(sqrt(recover_inertia(x)) ^ p[2])
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2)
    v <- v %*% s
  }
  return(v)
}

#' @rdname accessors
#' @export
get_factor <- function(x, .matrix, .supplement = TRUE) {
  switch(
    match_factor(.matrix),
    rows = get_rows(x, .supplement = .supplement),
    cols = get_cols(x, .supplement = .supplement),
    dims = list(
      rows = get_rows(x, .supplement = .supplement),
      cols = get_cols(x, .supplement = .supplement)
    )
  )
}

#' @rdname accessors
#' @export
as.matrix.tbl_ord <- function(
  x, ..., .matrix, .supplement = TRUE
) {
  .matrix <- match_factor(.matrix)
  if (.matrix == "dims")
    stop("Can only coerce one factor ('rows' or 'cols') to a matrix.")
  get_factor(x, .matrix = .matrix, .supplement = .supplement)
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
  intersect(colnames(recover_rows(x)), colnames(recover_cols(x)))
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
get_coord <- function(x) {
  recover_coord(x)
}

#' @rdname accessors
#' @export
dim.tbl_ord <- function(x) length(recover_coord(x))

#' @rdname accessors
#' @export
dim_rows <- function(x) nrow(recover_rows(x))

#' @rdname accessors
#' @export
dim_cols <- function(x) nrow(recover_cols(x))

#' @rdname accessors
#' @export
dim_factor <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    rows = dim_rows(x),
    cols = dim_cols(x),
    dims = c(rows = dim_rows(x), cols = dim_cols(x))
  )
}
