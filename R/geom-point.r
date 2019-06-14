#' @title Render points at ordinates
#' 

#' @description `geom_*_point()` renders symbols at the positions of the
#'   subjects or vectors.
#' @template ggbiplot-layers

#' @section Aesthetics:

#' `geom_*_point()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `fill`
#' - `linetype`
#' - `shape`
#' - `size`
#' - `stroke`
#' - `group`
#' 

#' @name ggbiplot-point
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template param-matrix
#' @example inst/examples/bioenv-lm-isolines.r
#' @example inst/examples/country-cmds-prcomp-negate.r
#' @example inst/examples/women-ca-confer.r
NULL

#' @rdname ggbiplot-point
#' @export
geom_u_point <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-point
#' @export
geom_v_point <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-point
#' @export
geom_biplot_point <- function(
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
    geom = GeomPoint,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
