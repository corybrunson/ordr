#' @title Unit circle
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
#' @param scale.factor The circle radius; should remain at its default value 1
#'   or passed the same value as [ggbiplot()]. (This is an imperfect fix that
#'   may be changed in a future version.)
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-unit-circle.r
#' @export
geom_unit_circle <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  segments = 60, scale.factor = 1,
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
      segments = segments, scale.factor = scale.factor,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomUnitCircle <- ggproto(
  "GeomUnitCircle", Geom,
  
  required_aes = c(),
  default_aes = aes(
    colour = "black", alpha = NA,
    linewidth = 0.5, linetype = 1
  ),
  
  setup_data = function(data, params) {
    # keep only columns that are constant throughout the data
    data <- dplyr::select_if(data, is_const)[1L, , drop = FALSE]
    rownames(data) <- NULL
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    segments = 60, scale.factor = 1
  ) {
    # check that data has been set up
    if (nrow(data) != 1L) stop("Constant-valued data has more than one row.")
    # remove any coordinates
    data$x <- NULL
    data$y <- NULL
    
    # unit circle as a path
    angles <- (0:segments) * 2 * pi/segments
    unit_circle <- data.frame(
      x = cos(angles) * scale.factor,
      y = sin(angles) * scale.factor,
      group = 1
    )
    
    # data frame of segments with aesthetics
    data <- cbind(unit_circle, data, row.names = NULL)
    # transform the coordinates into the viewport (iff using `polylineGrob()`)
    data <- coord$transform(data, panel_params)
    
    # return unit circle grob
    # GeomPath$draw_panel(
    #   data = data, panel_params = panel_params, coord = coord,
    #   na.rm = FALSE
    # )
    grob <- grid::polylineGrob(
      data$x, data$y,# id = NULL,
      default.units = "native",
      gp = grid::gpar(
        col = alpha(data$colour, data$alpha),
        fill = alpha(data$colour, data$alpha),
        lwd = (data$linewidth %||% data$size) * .pt,
        lty = data$linetype
      )
    )
    grob$name <- grid::grobName(grob, "geom_unit_circle")
    grob
  },
  
  draw_key = draw_key_blank,
  
  non_missing_aes = "size",
  rename_size = TRUE
)
