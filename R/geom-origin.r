#' @title Marker or unit circle at the origin
#' 
#' @description `geom_origin()` renders a symbol, either a set of crosshairs or
#'   a circle, at the origin. `geom_unit_circle()` renders the unit circle,
#'   centered at the origin with radius 1.

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_origin()` accepts no aesthetics.

#' `geom_unit_circle()` understands the following aesthetics (none required):

#' - `linetype`
#' - `linewidth`
#' - `colour`
#' - `alpha`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param marker The symbol to be drawn at the origin; matched to `"crosshairs"`
#'   or `"circle"`.
#' @param radius A [grid::unit()] object that sets the radius of the crosshairs
#'   or of the circle.
#' @param segments The number of segments to be used in drawing the circle.
#' @param scale.factor The circle radius; should remain at its default value 1
#'   or passed the same value as [ggbiplot()]. (This is an imperfect fix that
#'   may be changed in a future version.)
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-origin.r
#' @example inst/examples/ex-geom-unit-circle.r
#' @export
geom_origin <- function(
  mapping = NULL, data = NULL,# stat = "identity", position = "identity",
  marker = "crosshairs", radius = unit(0.04, "snpc"),
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = FALSE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomOrigin,
    position = PositionIdentity,
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
  default_aes = aes(
    linetype = 1, linewidth = 0.5,
    colour = "black", alpha = NA
  ),
  
  setup_data = function(data, params) {
    
    # one origin per panel, preserving columns that are constant within panels
    data <- tidyr::nest(data, aesthetics = -PANEL)
    data$aesthetics <- lapply(
      data$aesthetics,
      function(d) {
        dplyr::select(d, tidyselect::where(is_const))[1L, , drop = FALSE]
      }
    )
    data <- tidyr::unnest(data, aesthetics)
    
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
    
    # check that data has been set up (one row per panel)
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
      lwd = data$linewidth * .pt
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

#' @rdname geom_origin
#' @export
geom_unit_circle <- function(
    mapping = NULL, data = NULL,# stat = "identity", position = "identity",
    segments = 60, scale.factor = 1,
    ...,
    na.rm = FALSE,
    show.legend = NA, inherit.aes = FALSE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatIdentity,
    geom = GeomUnitCircle,
    position = PositionIdentity,
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
  "GeomUnitCircle", GeomOrigin,
  
  draw_panel = function(
    data, panel_params, coord,
    segments = 60, scale.factor = 1
  ) {
    # check that data has been set up
    if (nrow(data) != 1L) stop("Constant-valued data has more than one row.")
    # remove any coordinates
    data$x <- NULL
    data$y <- NULL
    data$group <- NULL
    
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
        lwd = data$linewidth * .pt,
        lty = data$linetype
      )
    )
    grob$name <- grid::grobName(grob, "geom_unit_circle")
    grob
  }
)
