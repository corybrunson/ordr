#' @title Render labels at the ends of axes
#' 

#' @description `geom_axis_label()` renders a text label outside the plotting
#'   window where it intersects each axis.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis_label()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - **`label`**
#' - `alpha`
#' - `angle`
#' - `colour`
#' - `family`
#' - `fontface`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @family geom layers
#' @example inst/examples/ex-geom-axis-diabetes.r
#' @export
geom_axis_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxisLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_axis_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAxisLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_axis_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAxisLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_axis_label <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxisLabel,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAxisLabel <- ggproto(
  "GeomAxisLabel", GeomText,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    save(data, params, file = "geom-axis-setup-data.rda")
    load("geom-axis-setup-data.rda")
    
    # diagonal versus vertical lines
    data$vline <- data$x == 0 & data$y != 0
    data$slope <- data$y / data$x
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    na.rm = FALSE
  ) {
    save(data, panel_params, coord, na.rm, file = "geom-axis-draw-panel.rda")
    load("geom-axis-draw-panel.rda")
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    # compute label positions
    data <- cbind(data, boundary_points(
      data$slope,
      panel_params$x.range, panel_params$y.range
    ))
    
    # label grob
    # grob <- grid::textGrob(
    #   data$label,
    #   data$x, data$y, default.units = "native",
    #   gp = grid::gpar(
    #     col = alpha(data$colour, data$alpha),
    #     fontsize = data$size * .pt
    #   )
    # )
    grob <- GeomText$draw_panel(
      data = data,
      panel_params = panel_params, coord = coord
    )
    grob$name <- grid::grobName(grob, "geom_axis_label")
    grob
  }
)

# -+- handle vertical and horizontal axes -+-
boundary_points <- function(slope, x.range, y.range) {
  res <- data.frame(slope = slope)
  # compute label positions
  res$increasing <- sign(res$slope) == 1L
  # (eventual) intersections with window borders
  res$a0 <- ifelse(
    res$increasing,
    y.range[[1L]],
    y.range[[2L]]
  ) / res$slope
  res$a1 <- ifelse(
    res$increasing,
    y.range[[2L]],
    y.range[[1L]]
  ) / res$slope
  res$b0 <- ifelse(
    res$increasing,
    x.range[[1L]],
    x.range[[2L]]
  ) * res$slope
  res$b1 <- ifelse(
    res$increasing,
    x.range[[2L]],
    x.range[[1L]]
  ) * res$slope
  # (bounded) intersections with window
  res$x0 <- pmax(x.range[[1L]], res$a0)
  res$x1 <- pmin(x.range[[2L]], res$a1)
  res$y0 <- pmax(x.range[[1L]], res$b0)
  res$y1 <- pmin(x.range[[2L]], res$b1)
  # farther intersection from origin
  res$x <- ifelse(
    abs(res$x0) < abs(res$x1),
    res$x1, res$x0
  )
  res$y <- ifelse(
    abs(res$y0) < abs(res$y1),
    res$y1, res$y0
  )
  res[, c("x", "y")]
}
