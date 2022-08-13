#' @title Access factors, coordinates, and metadata from ordination objects
#'
#' @description These functions return information about the matrix
#'   factorization underlying an ordination.
#'   

#' @details
#'
#' The `recover_*()` [S3 methods][base::S3Methods] extract one or both of the
#' row and column matrix factors that constitute the original ordination. These
#' are interpreted as the case scores (rows) and the variable loadings
#' (columns). The `get_*()` functions optionally (and by default) include any
#' supplemental observations (see [supplementation]).
#'
#' The `recover_*()` functions are generics that require methods for each
#' ordination class. They are not intended to be called directly but are
#' exported so that users can query `methods("recover_*")`.
#'
#' `get_coord()` retrieves the names of the coordinates shared by the matrix
#' factors on which the original data were ordinated, and `get_inertia()`
#' retrieves a vector of the inertia with these names. `dim()` retrieves the
#' dimensions of the row and column factors, which reflect the dimensions of the
#' matrix they reconstruct---**not** the original data matrix. (This matters for
#' techniques that rely on eigendecomposition, for which the decomposed matrix
#' is square.)
#' 

#' @name recoverers
#' @include utils.r
#' @param x An object of class '[tbl_ord]'.
#' @param ... Additional arguments from [base::as.matrix()]; ignored.
#' @template param-matrix
#' @template param-elements

#' @return The `recover_*()` functions are generics whose methods return base R
#'   objects retrieved from the model wrapped in the 'tbl_ord' class:

#'   - `rows`: the row matrix as stored in the model

#'   - `cols`: the column matrix as stored in the model

#'   - `inertia`: the vector of eigen-values or squared singular values,
#'     often known by other names depending on the model

#'   - `coord`: names for the artificial axes, from the model if available

#' The `get_*()` functions (which are not generics) return modifications of
#' these objects:

#'   - `rows`: the recovered rows,
#'     adjusted according to any negation of axes or conference of inertia

#'   - `cols`: the recovered columns,
#'     adjusted according to any negation of axes or conference of inertia

#'   - `inertia`: the recovered inertia, named by the recovered coordinates

#'   - `coord`: the recovered coordinates (unmodified)

#' `dim()` returns the dimensions of the decomposed matrix, i.e. the numbers of
#' rows of `recover_rows()` and of `recover_cols()`.

#' @family generic recoverers
#' @example inst/examples/ex-ord-recoverers.r
NULL

#' @rdname recoverers
#' @export
recover_rows <- function(x) UseMethod("recover_rows")

#' @rdname recoverers
#' @export
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

#' @rdname recoverers
#' @export
recover_rows.default <- function(x) x$rows

#' @rdname recoverers
#' @export
recover_cols.default <- function(x) x$cols

# for fortified tbl_ords (also coordinates?)

#' @rdname recoverers
#' @export
recover_rows.data.frame <- function(x) {
  x[x$.matrix == "rows", -match(".matrix", names(x))]
}

#' @rdname recoverers
#' @export
recover_cols.data.frame <- function(x) {
  x[x$.matrix == "cols", -match(".matrix", names(x))]
}

#' @rdname recoverers
#' @export
get_rows <- function(x, elements = "all") {
  # ensure that `elements` is a character singleton
  stopifnot(
    is.character(elements),
    length(elements) == 1L
  )
  # subset accordingly
  u <- if (elements == "all") {
    rbind(recover_rows(x), recover_supp_rows(x))
  } else if (elements == "active") {
    recover_rows(x)
  } else {
    # -+- need to recognize supplementary subtypes -+-
    recover_supp_rows(x)
  }
  # ensure correct signs
  if (! is.null(attr(x, "negate"))) {
    n <- diag(get_negation(x))
    dimnames(n) <- rep(list(recover_coord(x)), 2L)
    u <- u %*% n
  }
  # ensure correct distribution of inertia
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    i <- recover_inertia(x)
    s <- diag(sqrt(i) ^ p[[1L]], nrow = length(i), ncol = length(i))
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2L)
    u <- u %*% s
  }
  u
}

#' @rdname recoverers
#' @export
get_cols <- function(x, elements = "all") {
  # ensure that `elements` is a character singleton
  stopifnot(
    is.character(elements),
    length(elements) == 1L
  )
  # subset accordingly
  v <- if (elements == "all") {
    rbind(recover_cols(x), recover_supp_cols(x))
  } else if (elements == "active") {
    recover_cols(x)
  } else {
    # -+- need to recognize supplementary subtypes -+-
    recover_supp_cols(x)
  }
  # ensure correct signs
  if (! is.null(attr(x, "negate"))) {
    n <- diag(get_negation(x))
    dimnames(n) <- rep(list(recover_coord(x)), 2L)
    v <- v %*% n
  }
  # ensure correct distribution of inertia
  if (! is.null(attr(x, "confer"))) {
    p <- get_conference(x) - recover_conference(x)
    i <- recover_inertia(x)
    s <- diag(sqrt(i) ^ p[[2L]], nrow = length(i), ncol = length(i))
    # same coordinates (necessary for `ggbiplot()`)
    dimnames(s) <- rep(list(recover_coord(x)), 2L)
    v <- v %*% s
  }
  v
}

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

#' @rdname recoverers
#' @export
as.matrix.tbl_ord <- function(
  x, ..., .matrix, elements = "all"
) {
  .matrix <- match_factor(.matrix)
  if (.matrix == "dims")
    stop("Can only coerce one factor ('rows' or 'cols') to a matrix.")
  get_factor(x, .matrix = .matrix, elements = elements)
}

#' @rdname recoverers
#' @export
recover_inertia <- function(x) UseMethod("recover_inertia")

#' @rdname recoverers
#' @export
recover_inertia.default <- function(x) NA_real_

#' @rdname recoverers
#' @export
recover_coord <- function(x) UseMethod("recover_coord")

#' @rdname recoverers
#' @export
recover_coord.default <- function(x) {
  intersect(colnames(recover_rows(x)), colnames(recover_cols(x)))
}

#' @rdname recoverers
#' @export
recover_coord.data.frame <- function(x) {
  if (! is.null(attr(x, "coordinates"))) {
    attr(x, "coordinates")
  } else {
    recover_coord.default(x)
  }
}

#' @rdname recoverers
#' @export
get_coord <- function(x) {
  recover_coord(x)
}

#' @rdname recoverers
#' @export
get_inertia <- function(x) {
  `names<-`(recover_inertia(x), recover_coord(x))
}

#' @rdname recoverers
#' @export
dim.tbl_ord <- function(x) {
  c(nrow(recover_rows(x)), nrow(recover_cols(x)))
}
