#' @title Render vectors from origin to ordinates
#' 

#' @description `geom_*_vector()` renders arrows from the origin to the position
#'   of each case or variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_vector()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-vector
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template param-matrix
#' @param arrow Specification for arrows, as created by [grid::arrow()], or else
#'   `NULL` for no arrows.
#' @example inst/examples/iris-prcomp-lineranges.r
#' @example inst/examples/country-cmds-prcomp-negate.r
NULL

#' @rdname geom-biplot-vector
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

#' @rdname geom-biplot-vector
#' @export
geom_u_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
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

#' @rdname geom-biplot-vector
#' @export
geom_v_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
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

#' @rdname geom-biplot-vector
#' @export
geom_biplot_vector <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
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

default_arrow <- grid::arrow(
  angle = 30,
  length = unit(.02, "native"),
  ends = "last",
  type = "open"
)
