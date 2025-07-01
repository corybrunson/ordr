#' @title Vectors from the origin
#' 

#' @description `geom_vector()` renders arrows from the origin to points.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_vector()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param arrow Specification for arrows, as created by [grid::arrow()], or else
#'   `NULL` for no arrows.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-vector-iris.r
#' @export
geom_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomVector,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      arrow = arrow,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomVector <- ggproto(
  "GeomVector", GeomSegment,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    # all vectors have tails at the origin
    transform(
      data,
      xend = 0, yend = 0
    )
  },
  
  draw_panel = function(
    data, panel_params, coord,
    arrow = default_arrow,
    lineend = "round", linejoin = "mitre",
    na.rm = FALSE
  ) {
    
    if (! coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      na.rm = na.rm
    )
  }
)
