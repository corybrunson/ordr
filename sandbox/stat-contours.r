#' @title Compute intercepts and slopes of contour lines
#'
#' @description `stat_contours()` computes the intercepts and slopes of contour
#'   lines for one or more case or variable axes..
#'   

#' @template biplot-layers

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param axes Indices of axes for which to compute contours.
#' @param by Interval length between elements, in the units of the ordination.
#' @family stat layers
#' @export
stat_contours <- function(
  mapping = NULL, data = NULL, geom = "line", position = "identity",
  axes = NULL,
  show.legend = NA, 
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatContours,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      axes = axes,
      ...
    )
  )
}

#' @rdname biplot-stats
#' @export
stat_rows_countours <- function(
  mapping = NULL, data = NULL, geom = "line", position = "identity",
  axes = NULL,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRowsContours,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      axes = axes,
      ...
    )
  )
}

#' @rdname biplot-stats
#' @export
stat_cols_countours <- function(
  mapping = NULL, data = NULL, geom = "line", position = "identity",
  axes = NULL,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsContours,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      axes = axes,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatContours <- ggproto(
  "StatContours", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(
    data, scales
  ) {
    print(data)
    
    # by default, calculate contours for all axes
    if (! is.null(params$axes)) data <- data[params$axes, , drop = FALSE]
    
    # slopes of contours
    data$slope <- - data$x / data$y
    
    data
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsContours <- ggproto(
  "StatRowsContours", StatContours,
  
  setup_data = setup_rows_data
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsContours <- ggproto(
  "StatColsContours", StatContours,
  
  setup_data = setup_cols_data
)
