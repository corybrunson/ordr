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
#' @param u,v Matrices or data frames to be used as factors of a bibble.
#' @param coordinates Matrix, data frame, or character vector taken to be the
#'   shared coordinates of a bibble.
#' @param ... Parameters passed to other methods.
#' 

#' @rdname bibble
#' @export
as_bibble <- function(x) UseMethod("as_bibble")

as_bibble.bbl <- function(x) x

#' @rdname bibble
#' @export
make_bibble <- function(
  u = NULL, v = NULL, coordinates = NULL,
  ...
) {
  if (!all(coordinates %in% intersect(names(u), names(v)))) {
    stop("Coordinates must be fields shared by `u` and `v`.")
  }
  res <- list(
    u = u,
    v = v,
    coordinates = coordinates
  )
  class(res) <- c("bbl", "list")
  attr(res, "preclass") <- setdiff(class(x), "bbl")
  res
}

#' @rdname bibble
#' @export
is.bibble <- function(x) {
  res <- inherits(x, "bbl") &
    all(get_coordinates(x)$.name %in% names(get_u(x))) &
    all(get_coordinates(x)$.name %in% names(get_v(x))) &
    (!is.null(attr(x, "preclass")) | !is.null(setdiff(class(x), "bbl")))
  res
}
