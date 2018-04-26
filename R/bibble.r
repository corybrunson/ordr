#' A unified biplot object class

#' @name bibble
#' @param x An R object to be converted to or interpreted as a bibble.
#' @param u,v Matrices or data frames to be used as factors of a bibble.
#' @param coordinates Matrix, data frame, or character vector taken to be the
#'   shared coordinates of a bibble.
#' @param ... Parameters passed to other methods.

#' @rdname bibble
#' @export
bibble <- function(...) {
  x <- try(to_bibble(...))
  if (class(x) == "try-error") {
    return(make_bibble(...))
  } else {
    return(x)
  }
}

#' @rdname bibble
#' @export
make_bibble <- function(
  u = NULL, v = NULL, coordinates = NULL,
  ...
) {
  x <- c(list(u = u, v = v, coordinates = coordinates), list(...))
  attr(x, "preclass") <- NA
  to_bibble(x)
}

#' @rdname bibble
#' @export
as_bibble <- function(x) UseMethod("as_bibble")

as_bibble.bbl <- function(x) x

#' @rdname bibble
#' @export
to_bibble <- function(x) UseMethod("to_bibble")

to_bibble.default <- function(x) {
  res <- list(
    u = get_u(x),
    v = get_v(x),
    coordinates = get_coordinates(x)
  )
  class(res) <- "bbl"
  attr(res, "preclass") <- setdiff(class(x), "bbl")
  res
}

#' @rdname bibble
#' @export
is.bibble <- function(x) {
  res <- inherits(x, "bbl") &
    all(coord_names(x) %in% names(get_u(x))) &
    all(coord_names(x) %in% names(get_v(x))) &
    (!is.null(attr(x, "preclass")) | !is.null(setdiff(class(x), "bbl")))
  res
}
