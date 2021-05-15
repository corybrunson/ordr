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
  default_aes = aes(
    colour = "black", alpha = NA, size = 3.88, angle = 0,
    hjust = "inward", vjust = "inward",
    family = "", fontface = 1, lineheight = 1.2
  ),
  
  setup_data = function(data, params) {
    
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
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    # compute label positions
    data <- cbind(data, boundary_points(
      data$slope,
      panel_params$x.range, panel_params$y.range
    ))
    
    # ensure angles
    if (is.null(data$angle)) data$angle <- 0
    data$angle <- as.numeric(data$angle) + (180 / pi) * atan(data$y / data$x)
    
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
  res$a1 <- y.range[[1L]] / res$slope
  res$a2 <- y.range[[2L]] / res$slope
  res$b1 <- x.range[[1L]] * res$slope
  res$b2 <- x.range[[2L]] * res$slope
  # (bounded) intersections with window
  res$x1 <- pmax(x.range[[1L]], pmin(res$a1, res$a2))
  res$x2 <- pmin(x.range[[2L]], pmax(res$a1, res$a2))
  res$z1 <- pmax(y.range[[1L]], pmin(res$b1, res$b2))
  res$z2 <- pmin(y.range[[2L]], pmax(res$b1, res$b2))
  # account for negative slopes
  res$y1 <- ifelse(res$increasing, res$z1, res$z2)
  res$y2 <- ifelse(res$increasing, res$z2, res$z1)
  # distances from origin
  res$rsq1 <- res$x1 ^ 2 + res$y1 ^ 2
  res$rsq2 <- res$x2 ^ 2 + res$y2 ^ 2
  # farther intersection from origin
  res$x <- ifelse(res$rsq1 < res$rsq2, res$x2, res$x1)
  res$y <- ifelse(res$rsq1 < res$rsq2, res$y2, res$y1)
  res[, c("x", "y")]
}
