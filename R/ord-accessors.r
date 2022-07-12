#' @title Access factors, coordinates, and metadata from ordination objects
#'
#' @description These functions return information about the matrix
#'   factorization underlying an ordination.
#'   

#' @details
#'
#' The unexported `recover_*()` functions extract one or both of the row and
#' column matrix factors that constitute the original ordination. These are
#' interpreted as the case scores (rows) and the variable loadings (columns).
#' The `get_*()` functions optionally (and by default) include any supplemental
#' observations (see [supplementation]). The `recover_*()` functions are
#' generics that require methods for each ordination class.
#'
#' `get_coord()` retrieves the names of the coordinates shared by the matrix
#' factors on which the original data were ordinated, and `get_inertia()`
#' retrieves a vector of the inertia with these names. `dim()` retrieves their
#' number, the rank of the ordination. The outer dimensions of the matrix
#' decomposition are returned by `dim_rows()` and `dim_cols()`.
#' 

#' @name accessors
#' @include utils.r
#' @param x An object of class '[tbl_ord]'.
#' @param ... Additional arguments from [base::as.matrix()]; ignored.
#' @template param-matrix
#' @param elements Character; which elements of each factor for which to render
#'   graphical elements. One of `"all"` (the default), `"active"`, or
#'   `"supplementary"`, with partial matching.
#' @example inst/examples/ex-ord-accessors.r
NULL

recover_rows <- function(x) UseMethod("recover_rows")

recover_cols <- function(x) UseMethod("recover_cols")

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

recover_rows.default <- function(x) x$rows

recover_cols.default <- function(x) x$cols

# for fortified tbl_ords (also coordinates?)

recover_rows.data.frame <- function(x) {
  x[x$.matrix == "rows", -match(".matrix", names(x))]
}

recover_cols.data.frame <- function(x) {
  x[x$.matrix == "cols", -match(".matrix", names(x))]
}

#' @rdname accessors
#' @export
get_rows <- function(x, elements = "all") {
  elements <- match.arg(elements, c("all", "active", "supplementary"))
  u <- recover_rows(x)
  if (elements == "supplementary") {
    u <- supplementation_rows(x)
  } else if (elements == "all") {
    u <- rbind(u, supplementation_rows(x))
  }
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    i <- recover_inertia(x)
    s <- diag(sqrt(i) ^ p[[1L]], nrow = length(i), ncol = length(i))
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2L)
    u <- u %*% s
  }
  return(u)
}

#' @rdname accessors
#' @export
get_cols <- function(x, elements = "all") {
  elements <- match.arg(elements, c("all", "active", "supplementary"))
  v <- recover_cols(x)
  if (elements == "supplementary") {
    v <- supplementation_cols(x)
  } else if (elements == "all") {
    v <- rbind(v, supplementation_cols(x))
  }
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    i <- recover_inertia(x)
    s <- diag(sqrt(i) ^ p[[2L]], nrow = length(i), ncol = length(i))
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2L)
    v <- v %*% s
  }
  return(v)
}

#' @rdname accessors
#' @export
get_factor <- function(x, .matrix, elements = "all") {
  switch(
    match_factor(.matrix),
    rows = get_rows(x, elements = elements),
    cols = get_cols(x, elements = elements),
    dims = list(
      rows = get_rows(x, elements = elements),
      cols = get_cols(x, elements = elements)
    )
  )
}

#' @rdname accessors
#' @export
as.matrix.tbl_ord <- function(
  x, ..., .matrix, elements = "all"
) {
  .matrix <- match_factor(.matrix)
  if (.matrix == "dims")
    stop("Can only coerce one factor ('rows' or 'cols') to a matrix.")
  get_factor(x, .matrix = .matrix, elements = elements)
}

recover_inertia <- function(x) UseMethod("recover_inertia")

recover_inertia.default <- function(x) NA_real_

recover_coord <- function(x) UseMethod("recover_coord")

recover_coord.default <- function(x) {
  intersect(colnames(recover_rows(x)), colnames(recover_cols(x)))
}

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
get_inertia <- function(x) {
  `names<-`(recover_inertia(x), recover_coord(x))
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
