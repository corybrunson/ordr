#' @title Render a unit circle
#' 

#' @description `geom_unit_circle()` renders the unit circle, centered at the
#'   origin with radius 1.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_unit_circle()` understands the following aesthetics (none required):

#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param segments The number of segments to be used in drawing the circle.
#' @family geom layers
#' @example inst/examples/ex-geom-unit-circle.r
#' @export
geom_unit_circle <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  segments = 60,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomUnitCircle,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      segments = segments,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomUnitCircle <- ggproto(
  "GeomUnitCircle", GeomPath,
  
  required_aes = c(),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = NA),
  
  draw_panel = function(
    data, panel_params, coord,
    segments = 60
  ) {
    # first row (for aesthetics)
    first_row <- data[1, setdiff(names(data), c("x", "y")), drop = FALSE]
    rownames(first_row) <- NULL
    
    # unit circle as a path
    angles <- (0:segments) * 2 * pi/segments
    unit_circle <- data.frame(x = cos(angles), y = sin(angles), group = 1)
    
    # data frame of segments with aesthetics
    data <- cbind(unit_circle, first_row)
    
    # return unit circle grob
    GeomPath$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      na.rm = FALSE
    )
  },
  
  draw_key = draw_key_blank
)
