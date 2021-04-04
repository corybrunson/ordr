#' @title Render crosshairs on a plot
#' 

#' @description `geom_crosshairs()` renders a set of crosshairs, by default
#'   located at the origin.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_crosshairs()` accepts no aesthetics.

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param length A [grid::unit()] object that sets the length of the crosshairs.
#'   Use scale expansion to avoid overplotting of data.
#' @family geom layers
#' @export
geom_crosshairs <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  length = unit(0.04, "snpc"),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomCrosshairs,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      length = length,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomCrosshairs <- ggproto(
  "GeomCrosshairs", Geom,
  
  required_aes = c(),
  default_aes = aes(colour = "black", size = 0.5, linetype = 1, alpha = .75),
  
  setup_data = function(data, params) {
    # keep only columns that are constant throughout the data
    data <- dplyr::select_if(data, is_const)[1L, , drop = FALSE]
    rownames(data) <- NULL
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    length = unit(0.04, "snpc")
  ) {
    if (! inherits(length, "unit")) {
      abort("`length` must be a 'unit' object.")
    }
    
    # check that data has been set up
    if (nrow(data) != 1L) stop("Constant-valued data has more than one row.")
    # origin coordinates
    data$x <- 0
    data$y <- 0
    # transform the origin into the viewport
    data <- coord$transform(data, panel_params)
    
    # common graphical parameters for both crosshairs
    crosshairs <- list()
    gp <- grid::gpar(
      col = alpha(data$colour, data$alpha),
      lty = data$linetype,
      lwd = data$size * .pt
    )
    crosshairs$x <- grid::segmentsGrob(
      x0 = unit(data$x, "native") - length,
      y0 = unit(data$y, "native"),
      x1 = unit(data$x, "native") + length,
      y1 = unit(data$y, "native"),
      gp = gp
    )
    crosshairs$y <- grid::segmentsGrob(
      x0 = unit(data$x, "native"),
      y0 = unit(data$y, "native") - length,
      x1 = unit(data$x, "native"),
      y1 = unit(data$y, "native") + length,
      gp = gp
    )
    
    #grid::gTree(children = do.call(grid::gList, crosshairs))
    grob <- do.call(grid::grobTree, crosshairs)
    grob$name <- grid::grobName(grob, "geom_crosshairs")
    grob
  },
  
  draw_key = draw_key_blank
)
