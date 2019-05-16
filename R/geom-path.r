#' Render piecewise-linear paths through ordinates
#' 

#' \code{geom_*_path} renders line segments through consecutive pairs of case or
#' variable positions.
#' @template ggbiplot-layers

#' @section Aesthetics:
#' \code{geom_*_path} understands the following aesthetics (required aesthetics
#' are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{group}
#' }
#' 

#' @name ggbiplot-path
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-layer
#' @template param-matrix
#' @example inst/examples/ex-lm.r

#' @rdname ggbiplot-path
#' @export
geom_u_path <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-path
#' @export
geom_v_path <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-path
#' @export
geom_biplot_path <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "u",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomPath,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
