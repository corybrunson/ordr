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
#' methods being equivalent (simply appending tbl_ord to the vector of object
#' classes). This prevents objects for which other methods are not defined from
#' being re-classed as tbl_ords.
#'
#' The function `make_tbl_ord()` creates a tbl_ord structured as a list of two
#' matrices, `u` and `v`, which must have the same number of columns and the
#' same column names.
#'
#' `is_tbl_ord()` checks an object `x` for the tbl_ord class; `valid_tbl_ord()`
#' additionally checks for consistency between `recover_coord(x)` and the
#' columns of `recover_u(x)` and `recover_v(x)`, using the [accessors].
#' `un_tbl_ord()` removes attributes associated with the tbl_ord class in order
#' to restore an object that was originally passed to `as_tbl_ord`.
#' 

#' @name tbl_ord
#' @include ord-alignment.r ord-conference.r
#' @importFrom tibble tibble is_tibble as_tibble
#' @param x An ordination object.
#' @param u,v Matrices to be used as factors of a tbl_ord.
#' @param ... Additional elements of a custom tbl_ord.
NULL

#' @rdname tbl_ord
#' @export
as_tbl_ord <- function(x) UseMethod("as_tbl_ord")

#' @rdname tbl_ord
#' @export
as_tbl_ord.tbl_ord <- function(x) x

#' @rdname tbl_ord
#' @export
make_tbl_ord <- function(u = NULL, v = NULL, ...) {
  if (!is.matrix(u) || !is.matrix(v) || ncol(u) == ncol(v)) {
    stop("`u` and `v` must be matrices having the same number of columns.")
  }
  if (!is.null(colnames(u)) & !is.null(colnames(v))) {
    if (any(colnames(u) != colnames(v))) {
      stop("`u` and `v` must have the same column names.")
    }
  }
  res <- list(u = u, v = v, ...)
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
  if (! is.null(attr(x, "align")) &&
      ! is.matrix(attr(x, "align")) &&
      ! all(dim(attr(x, "align") == rep(dim(x), 2)))) return(FALSE)
  if (! is.null(attr(x, "u_annotation")) &&
      ! is_tibble(attr(x, "u_annotation"))) return(FALSE)
  if (! is.null(attr(x, "v_annotation")) &&
      ! is_tibble(attr(x, "v_annotation"))) return(FALSE)
  if (! is.null(attr(x, "confer")) &&
      (! is.numeric(attr(x, "confer")) ||
       length(attr(x, "confer")) != 2)) return(FALSE)
  if (is.null(recover_coord(x)) ||
      is.null(recover_u(x)) ||
      is.null(recover_v(x))) return(FALSE)
  if (! all(recover_coord(x) %in% colnames(recover_u(x)))) return(FALSE)
  if (! all(recover_coord(x) %in% colnames(recover_v(x)))) return(FALSE)
  TRUE
}

#' @rdname tbl_ord
#' @export
un_tbl_ord <- function(x) {
  if (! is_tbl_ord(x)) return(x)
  attr(x, "align") <- NULL
  attr(x, "u_annotation") <- NULL
  attr(x, "v_annotation") <- NULL
  attr(x, "confer") <- NULL
  class(x) <- setdiff(class(x), "tbl_ord")
  x
}
