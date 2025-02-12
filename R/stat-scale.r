#' @title Multiply artificial coordinates by a scale factor
#' 
#' @description
#' This layer is **deprecated**.
#' 

#' @template biplot-layers
#' @template biplot-ord-aes

#' @inheritParams ggplot2::layer
#' @inheritParams stat_rows
#' @param mult Numeric value used to scale the coordinates.
#' @template param-stat
#' @template return-layer
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

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatScale <- ggproto(
  "StatScale", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales, mult = 1) {
    warning("`StatScale` is deprecated and will be removed next release.")
    data[, c("x", "y")] <- data[, c("x", "y")] * mult
    data
  }
)
