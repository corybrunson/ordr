#' Render axes through origin
#' 

#' \code{geom_*_axis} renders lines through the origin and the position of each
#' case or variable.
#' @template ggbiplot-layers

#' @section Aesthetics:
#' \code{geom_*_axis} understands the following aesthetics (required
#' aesthetics are in bold):
#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{group}
#' }
#' 

#' @name ggbiplot-axis
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-layer
#' @template param-matrix

#' @rdname ggbiplot-axis
#' @usage NULL
#' @export
GeomAxis <- ggproto(
  "GeomAxis", GeomAbline,
  
  required_aes = c("x", "y"),
  
  draw_panel = function(
    data, panel_params, coord,
    na.rm = FALSE
  ) {
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    ranges <- coord$range(panel_params)
    
    data$slope <- data$y / data$x
    data$vline <- data$y == 0
    data$x <- ifelse(data$vline, 0, ranges$x[1])
    data$xend <- ifelse(data$vline, 0, ranges$x[2])
    data$y <- ifelse(data$vline, ranges$y[1], ranges$x[1] * data$slope)
    data$yend <- ifelse(data$vline, ranges$y[2], ranges$x[2] * data$slope)
    
    GeomSegment$draw_panel(
      data = unique(data), panel_params = panel_params, coord = coord,
      na.rm = na.rm
    )
  }
)

#' @rdname ggbiplot-axis
#' @export
geom_u_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-axis
#' @export
geom_v_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-axis
#' @export
geom_biplot_axis <- function(
  mapping = NULL, data = NULL, position = "identity",
  .matrix = "v",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = .matrix,
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}