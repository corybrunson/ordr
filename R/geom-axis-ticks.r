#' @title Render tick marks for axes
#'
#' @description `geom_axis_ticks()` renders tick marks for specified axes among
#'   the row or column factors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis_ticks()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' - `center` (for un-scaling)
#' - `scale` (for un-scaling)
#' 

#' @import ggplot2
#' @include geom-isolines.r
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @inheritParams geom_isolines
#' @param tick_length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @family geom layers
#' @example inst/examples/ex-geom-axis-diabetes.r
#' @export
geom_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  subset = NULL, by = NULL, num = NULL, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAxisTicks <- ggproto(
  "GeomAxisTicks", GeomSegment,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", alpha = NA, size = .25, linetype = "solid"
  ),
  
  setup_params = GeomIsolines$setup_params,
  setup_data = GeomIsolines$setup_data,
  
  draw_panel = function(
    data, panel_params, coord,
    subset = NULL, by = NULL, num = NULL, tick_length = .025
  ) {
    if (is.null(by) && is.null(num)) num <- 6L
    
    ranges <- coord$range(panel_params)
    
    data <- calibrate_axes(data, ranges, by, num)
    
    # tick mark radius
    rtick <- min(diff(ranges$x), diff(ranges$y)) * tick_length / 2
    # tick mark vector
    data <- transform(
      data,
      xtick = - axis_y / sqrt(axis_ss) * rtick,
      ytick = axis_x / sqrt(axis_ss) * rtick
    )
    # endpoints of tick marks
    data <- transform(
      data,
      x = x_val - xtick, xend = x_val + xtick,
      y = y_val - ytick, yend = y_val + ytick
    )
    # discard unneeded columns
    data$xtick <- NULL
    data$ytick <- NULL
    
    grob <- GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
    grob$name <- grid::grobName(grob, "geom_axis_ticks")
    grob
  }
)
