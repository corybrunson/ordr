#' @title Render points at ordinates
#' 

#' @description `geom_*_point()` renders symbols at the positions of the
#'   subjects or vectors.
#' @template biplot-layers

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

#' @name geom-biplot-point
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template param-matrix
NULL

#' @rdname geom-biplot-point
#' @export
geom_rows_point <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
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

#' @rdname geom-biplot-point
#' @export
geom_cols_point <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
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

#' @rdname geom-biplot-point
#' @export
geom_dims_point <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "rows",
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
