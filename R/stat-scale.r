#' @title Multiply artificial coordinates by a scale factor
#'

#' @template biplot-layers

#' @inheritParams ggplot2::layer
#' @param mult Numeric value used to scale the coordinates.
#' @template param-stat
#' @family stat layers
#' @export
stat_scale <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  mult = 1
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatScale,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      mult = mult,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname biplot-stats
#' @export
stat_rows_scale <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  mult = 1
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRowsScale,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      mult = mult,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname biplot-stats
#' @export
stat_cols_scale <- function(
  mapping = NULL, data = NULL, geom = "point", position = "identity",
  show.legend = NA,
  inherit.aes = TRUE,
  ...,
  mult = 1
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsScale,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      mult = mult,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatScale <- ggproto(
  "StatScale", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           mult = 1) {
    
    # columns to scale (see also `StatSpantree`)
    scale_cols <- grep("^\\.coord[0-9]+$", names(data))
    if (length(scale_cols) == 0) scale_cols <- match(c("x", "y"), names(data))
    
    # scale designated columns
    data[, scale_cols] <- data[, scale_cols] * mult
    
    data
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsScale <- ggproto(
  "StatRowsScale", StatScale,
  
  setup_data = setup_rows_data
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsScale <- ggproto(
  "StatColsScale", StatScale,
  
  setup_data = setup_cols_data
)
