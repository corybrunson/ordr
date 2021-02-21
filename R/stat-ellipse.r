#' @title Compute normal confidence ellipses for ordination factors
#'
#' @description These ordination stats are adapted from
#'   [ggplot2::stat_ellipse()].
#'   

#' @template biplot-layers

#' @name stat-biplot-ellipse
#' @inheritParams ggplot2::stat_ellipse
#' @template param-stat
#' @example inst/examples/iris-prcomp-center-ellipse.r
NULL

#' @rdname stat-biplot-ellipse
#' @usage NULL
#' @export
StatRowsEllipse <- ggproto(
  "StatRowsEllipse", StatEllipse,
  
  setup_data = setup_rows_data
)

#' @rdname stat-biplot-ellipse
#' @usage NULL
#' @export
StatColsEllipse <- ggproto(
  "StatColsEllipse", StatEllipse,
  
  setup_data = setup_cols_data
)

#' @rdname stat-biplot-ellipse
#' @export
stat_rows_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRowsEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}

#' @rdname stat-biplot-ellipse
#' @export
stat_cols_ellipse <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  type = "t",
  level = 0.95,
  segments = 51
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsEllipse,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...,
      type = type,
      level = level,
      segments = segments
    )
  )
}
