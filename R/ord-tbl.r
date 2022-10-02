#' @title A unified ordination object class
#'
#' @description These functions wrap ordination objects in the class [tbl_ord],
#'   create tbl_ords directly from matrices, and test for the class and basic
#'   structure.
#'   

#' @details
#'
#' The tbl_ord class wraps around a range of ordination classes, making
#' available a suite of ordination tools that specialize to each original object
#' class. These tools include [format()] and [fortify()] methods, which
#' facilitate the [print()] method and the [ggbiplot()] function.
#'
#' No default method is provided for `as_tbl_ord()`, despite most defined
#' methods being equivalent (simply appending 'tbl_ord' to the vector of object
#' classes). This prevents objects for which other methods are not defined from
#' being re-classed as tbl_ords.
#'
#' The function `make_tbl_ord()` creates a tbl_ord structured as a list of two
#' matrices, `u` and `v`, which must have the same number of columns and the
#' same column names.
#'
#' `is_tbl_ord()` checks an object `x` for the tbl_ord class; `valid_tbl_ord()`
#' additionally checks for consistency between `recover_coord(x)` and the
#' columns of `recover_rows(x)` and `recover_cols(x)`, using the [recoverers].
#' `un_tbl_ord()` removes attributes associated with the tbl_ord class in order
#' to restore an object that was originally passed to `as_tbl_ord`.
#' 

#' @name tbl_ord
#' @include ord-conference.r
#' @importFrom tibble tibble is_tibble as_tibble
#' @param x An ordination object.
#' @param rows,cols Matrices to be used as factors of a tbl_ord.
#' @param ... Additional elements of a custom tbl_ord.
#' @return A tbl_ord (`as*()`, `make*()`), an S3-class model object that can be
#'   wrapped as one (`un*()`), or a logical value (`is*()`, `value*()`).
#' @example inst/examples/ex-ord-tbl.r
NULL

#' @rdname tbl_ord
#' @export
as_tbl_ord <- function(x) UseMethod("as_tbl_ord")

#' @rdname tbl_ord
#' @export
as_tbl_ord.tbl_ord <- function(x) x

#' @rdname tbl_ord
#' @export
make_tbl_ord <- function(rows = NULL, cols = NULL, ...) {
  if (! is.matrix(rows) || ! is.matrix(cols) || ncol(rows) != ncol(cols)) {
    stop("`rows` and `cols` must be matrices having the same number of columns.")
  }
  if (! is.null(colnames(rows)) & ! is.null(colnames(cols))) {
    if (any(colnames(rows) != colnames(cols))) {
      stop("`rows` and `cols` must have the same column names.")
    }
  }
  res <- list(rows = rows, cols = cols, ...)
  class(res) <- c("tbl_ord", class(res))
  res
}

#' @rdname tbl_ord
#' @export
is_tbl_ord <- function(x) inherits(x, "tbl_ord")

#' @rdname tbl_ord
#' @export
is.tbl_ord <- is_tbl_ord

#' @rdname tbl_ord
#' @export
valid_tbl_ord <- function(x) {
  if (! is_tbl_ord(x)) return(FALSE)
  if (! is.null(attr(x, "rows_annotation")) &&
      ! is_tibble(attr(x, "rows_annotation"))) return(FALSE)
  if (! is.null(attr(x, "cols_annotation")) &&
      ! is_tibble(attr(x, "cols_annotation"))) return(FALSE)
  # -+- update this check for eigendecomposition-based and 3-factor ordinations
  if (! is.null(attr(x, "confer")) &&
      (! is.numeric(attr(x, "confer")) ||
       length(attr(x, "confer")) != 2)) return(FALSE)
  if (is.null(recover_coord(x)) ||
      is.null(recover_rows(x)) ||
      is.null(recover_cols(x))) return(FALSE)
  if (! all(recover_coord(x) %in% colnames(recover_rows(x)))) return(FALSE)
  if (! all(recover_coord(x) %in% colnames(recover_cols(x)))) return(FALSE)
  TRUE
}

#' @rdname tbl_ord
#' @export
un_tbl_ord <- function(x) {
  if (! is_tbl_ord(x)) return(x)
  attr(x, "rows_annotation") <- NULL
  attr(x, "cols_annotation") <- NULL
  attr(x, "confer") <- NULL
  class(x) <- setdiff(class(x), "tbl_ord")
  x
}
