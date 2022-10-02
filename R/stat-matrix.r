#' @title Render plot elements for one matrix of an ordination
#'
#' @description These stats merely tell [ggplot2::ggplot()] which factor of an
#'   ordination to pull data from for a plot layer. They are invoked internally
#'   by the various [`geom_*_*()`][biplot-geoms] layers.
#'   

#' @template biplot-layers

#' @name stat_rows
#' @aliases stat_cols
#' @inheritParams ggplot2::layer
#' @template param-elements
#' @param subset An integer, logical, or character vector indicating a subset of
#'   rows or columns for which to render graphical elements. NB: Internally, the
#'   `subset` will be taken from the rows of the [fortified][fortify.tbl_ord()]
#'   'tbl_ord' comprising rows from only one of the matrix factors. It is still
#'   possible to pass a formula to the `data` parameter, but it will act on the
#'   fortified data _before_ it has been restricted to one matrix factor.
#' @template param-stat
#' @template return-layer
#' @family biplot layers
#' @example inst/examples/ex-stat-matrix-swiss.r

#' @rdname stat_rows
#' @export
stat_rows <- function(
  mapping = NULL, data = data,
  geom = "point", position = "identity",
  subset = NULL, elements = "all",
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
      subset = subset, elements = elements,
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
  subset = NULL, elements = "all",
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
      subset = subset, elements = elements,
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
  
  setup_data = setup_rows_data,
  
  compute_group = function(data, scales,
                           subset = NULL, elements = "all") {
    data
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCols <- ggproto(
  "StatCols", StatIdentity,
  
  setup_data = setup_cols_data,
  
  compute_group = function(data, scales,
                           subset = NULL, elements = "all") {
    data
  }
)
