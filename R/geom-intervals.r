#' @title Render intervals depicting ranges, usually about center points
#' 

#' @description `geom_*_lineranges()` renders horizontal and vertical intervals
#'   for a specified subject or variable; `geom_*_pointranges()` additionally
#'   renders a point at their crosshairs.
#' @template ggbiplot-layers

#' @section Aesthetics:

#' `geom_*_lineranges()` and `geom_*_pointranges()` understand the following
#' aesthetics (required aesthetics are in bold):

#' - **`x`**
#' - **`xmin`**
#' - **`xmax`**
#' - **`y`**
#' - **`ymin`**
#' - **`ymax`**`
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name ggbiplot-intervals
#' @include biplot-legend.r
#' @import ggplot2
#' @inheritParams ggplot2::geom_linerange
#' @template param-geom
#' @template param-matrix
#' @example inst/examples/iris-prcomp-lineranges.r

#' @rdname ggbiplot-intervals
#' @usage NULL
#' @export
GeomLineranges <- ggproto(
  "GeomLineranges", GeomLinerange,
  
  required_aes = c("x", "xmin", "xmax", "y", "ymin", "ymax"),
  
  draw_key = draw_key_crosslines,
  
  draw_panel = function(data, panel_params, coord) {
    x_data <- transform(data, x = xmin, xend = xmax, yend = y)
    y_data <- transform(data, xend = x, y = ymin, yend = ymax)
    data <- rbind(x_data, y_data)
    
    grob <- GeomSegment$draw_panel(data, panel_params, coord)
    grob$name <- grid::grobName(grob, "geom_lineranges")
    grob
  }
)

#' @rdname ggbiplot-intervals
#' @export
geom_u_lineranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomLineranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-intervals
#' @export
geom_v_lineranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomLineranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-intervals
#' @export
geom_biplot_lineranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  .matrix = "u",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomLineranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-intervals
#' @usage NULL
#' @export
GeomPointranges <- ggproto(
  "GeomPointranges", GeomPointrange,
  
  required_aes = c("x", "xmin", "xmax", "y", "ymin", "ymax"),
  
  draw_key = draw_key_crosspoint,
  
  draw_panel = function(data, panel_params, coord, flatten = 4) {
    pt_data <- data
    pt_data$size = pt_data$size * flatten
    
    grob <- grid::gTree(children = grid::gList(
      GeomLineranges$draw_panel(data, panel_params, coord),
      GeomPoint$draw_panel(pt_data, panel_params, coord)
    ))
    grob$name <- grid::grobName(grob, "geom_pointranges")
    grob
  }
)

#' @rdname ggbiplot-intervals
#' @export
geom_u_pointranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomPointranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-intervals
#' @export
geom_v_pointranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomPointranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ggbiplot-intervals
#' @export
geom_biplot_pointranges <- function(
  mapping = NULL, data = NULL, stat = "center", position = "identity",
  .matrix = "u",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomPointranges,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
