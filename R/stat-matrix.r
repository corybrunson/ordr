#' @title Render plot elements for one matrix of an ordination
#'
#' @description These stats merely tell [ggplot2::ggplot()] which factor of an
#'   ordination to pull data from for a plot layer. They are invoked internally
#'   by the various [`geom_*_*()`][biplot-geoms] layers.
#'   

#' @template biplot-layers

#' @name stat_rows
#' @inheritParams ggplot2::layer
#' @template param-stat
#' @family biplot layers

#' @rdname stat_rows
#' @export
stat_rows <- function(
  mapping = NULL, data = data,
  geom = "point", position = "identity",
  ...,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "rows",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname stat_rows
#' @export
stat_cols <- function(
  mapping = NULL, data = data,
  geom = "axis", position = "identity",
  ...,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "cols",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRows <- ggproto(
  "StatRows", StatIdentity,
  
  setup_data = setup_rows_data
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCols <- ggproto(
  "StatCols", StatIdentity,
  
  setup_data = setup_cols_data
)
