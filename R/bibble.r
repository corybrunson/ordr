#' A unified ordination object class
#' 
#' The \code{bibble} class wraps around a range of ordination classes, making 
#' available a suite of ordination tools that specialize to each original object
#' class, including \code{print}, a variety of \strong{dplyr} verbs, and
#' \code{fortify} (a precursor to \code{\link{ggbiplot}}).
#' 
#' Note that no default method is provided for \code{as_bibble}, despite most 
#' defined methods being equivalent. This is to prevent objects for which other 
#' methods are not defined from being re-classed as bibbles.
#' 

#' @name bibble
#' @param x An ordination object.
#' @param u,v Matrices to be used as factors of a bibble.
#' @param coord Character vector taken to be the shared coordinates of \code{u}
#'   and \code{v}.
#' @param ... Parameters passed to other methods.
#'   

#' @rdname bibble
#' @export
as_bibble <- function(x) UseMethod("as_bibble")

as_bibble.bbl <- function(x) x

#' @rdname bibble
#' @export
make_bibble <- function(u = NULL, v = NULL, ...) {
  if (!is.matrix(u) || !is.matrix(v) || ncol(u) == ncol(v)) {
    stop("`u` and `v` must be matrices having the same number of columns.")
  }
  if (!is.null(colnames(u)) & !is.null(colnames(v))) {
    if (any(colnames(u) != colnames(v))) {
      stop("`u` and `v` must have the same column names.")
    }
  }
  res <- list(u = u, v = v)
  class(res) <- c("bbl", class(res))
  res
}

#' @rdname bibble
#' @export
is.bibble <- function(x) {
  if (!inherits(x, "bbl")) return(FALSE)
  if (is.null(get_coord(x)) ||
      is.null(get_u(x)) ||
      is.null(get_v(x))) return(FALSE)
  if (!all(get_coord(x) %in% colnames(get_u(x)))) return(FALSE)
  if (!all(get_coord(x) %in% colnames(get_v(x)))) return(FALSE)
  TRUE
}
