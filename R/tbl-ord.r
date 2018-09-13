#' A unified ordination object class
#' 
#' These functions wrap ordination objects in the \code{"tbl_ord"} class, create
#' \code{tbl_ord}s directly from matrices, and test for the class and basic 
#' structure.
#' 

#' The \code{"tbl_ord"} class wraps around a range of ordination classes, making
#' available a suite of ordination tools that specialize to each original object
#' class, including \code{\link{format}} and \code{\link{fortify}}, which
#' facilitate the \code{\link{print}} method and the \code{\link{ggbiplot}}
#' function.
#'
#' No default method is provided for \code{as_tbl_ord}, despite most defined
#' methods being equivalent (simply adding \code{"tbl_ord"} to the vector of
#' object classes). This prevents objects for which other methods are not
#' defined from being re-classed as \code{tbl_ord}s.
#'
#' The function \code{make_tbl_ord} creates a \code{tbl_ord} structured as a
#' list of two matrices, \code{u} and \code{v}, which must have the same number
#' of columns and the same column names.
#'
#' \code{is_tbl_ord} checks an object \code{x} for the \code{"tbl_ord"} class;
#' \code{valid_tbl_ord} additionally checks for consistency between
#' \code{recover_coord(x)} and the columns of \code{recover_u(x)} and
#' \code{recover_v(x)}, using the functions at \code{\link{accessors}}.
#' \code{un_tbl_ord} removes attributes associated with the \code{"tbl_ord"}
#' class in order to restore an object that was originally passed to
#' \code{as_tbl_ord}.
#' 

#' @name tbl_ord
#' @include alignment.r
#' @importFrom tibble tibble is_tibble as_tibble
#' @param x An ordination object.
#' @param u,v Matrices to be used as factors of a \code{tbl_ord}.
#' @param ... Additional elements of a custom \code{tbl_ord}.
#'   

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
