#' @title Crosshairs or circle at the origin
#' 

#' @description `geom_origin()` renders a symbol, either a set of crosshairs or
#'   a circle, at the origin.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_origin()` accepts no aesthetics.

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param marker The symbol to be drawn at the origin; matched to `"crosshairs"`
#'   or `"circle"`.
#' @param radius A [grid::unit()] object that sets the radius of the crosshairs
#'   or of the circle.
#' @template return-layer
#' @family geom layers
#' @export
geom_origin <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  marker = "crosshairs", radius = unit(0.04, "snpc"),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomOrigin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      marker = marker,
      radius = radius,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomOrigin <- ggproto(
  "GeomOrigin", Geom,
  
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
    marker = "crosshairs", radius = unit(0.04, "snpc")
  ) {
    
    marker <- match.arg(marker, c("crosshairs", "circle"))
    if (! inherits(radius, "unit")) {
      abort("`radius` must be a 'unit' object.")
    }
    
    # check that data has been set up
    if (nrow(data) != 1L) stop("Constant-valued data has more than one row.")
    # origin coordinates
    data$x <- 0
    data$y <- 0
    # transform the origin into the viewport
    data <- coord$transform(data, panel_params)
    
    # common graphical parameters for either marker (except `fill`)
    gp <- grid::gpar(
      col = alpha(data$colour, data$alpha),
      fill = NA,
      lty = data$linetype,
      lwd = data$size * .pt
    )
    if (marker == "crosshairs") {
      # list of grobs
      origin <- list()
      # crosshair coordinates
      origin$x <- grid::segmentsGrob(
        x0 = unit(data$x, "native") - radius,
        y0 = unit(data$y, "native"),
        x1 = unit(data$x, "native") + radius,
        y1 = unit(data$y, "native"),
        gp = gp
      )
      origin$y <- grid::segmentsGrob(
        x0 = unit(data$x, "native"),
        y0 = unit(data$y, "native") - radius,
        x1 = unit(data$x, "native"),
        y1 = unit(data$y, "native") + radius,
        gp = gp
      )
      # grob tree
      grob <- do.call(grid::grobTree, origin)
    } else if (marker == "circle") {
      # circle grob
      grob <- grid::circleGrob(
        x = unit(data$x, "native"),
        y = unit(data$y, "native"),
        r = radius,
        gp = gp
      )
    }
    
    grob$name <- grid::grobName(grob, "geom_origin")
    grob
  },
  
  draw_key = draw_key_blank
)
