#' @title Render plot elements for one matrix of an ordination
#'
#' @description These stats merely tell [ggplot2::ggplot()] which factor of an
#'   ordination to pull data from for a plot layer. They are invoked internally
#'   by the various [`geom_*_*()`][biplot-geoms] layers.
#'   

#' @template biplot-layers

#' @name stat_rows
#' @inheritParams ggplot2::layer
#' @param subset An integer, logical, or character vector indicating a subset of
#'   rows or columns for which to render graphical elements. NB: Internally, the
#'   `subset` will be taken from the rows of the [fortified][fortify.tbl_ord()]
#'   'tbl_ord' comprising rows from only one of the matrix factors. It is still
#'   possible to pass a formula to the `data` parameter, but it will act on the
#'   fortified data _before_ it has been restricted to one matrix factor.
#' @param supplementary Logical; whether to restrict to primary (`FALSE`) or
#'   [supplementary][supplementation] (`TRUE`) elements. Defaults to `NA`, which
#'   makes no restriction.
#'
#' @template param-stat
#' @family biplot layers

#' @rdname stat_rows
#' @export
stat_rows <- function(
  mapping = NULL, data = data,
  geom = "point", position = "identity",
  subset = NULL, supplementary = NA,
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
      subset = subset, supplementary = supplementary,
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
  subset = NULL, supplementary = NA,
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
      subset = subset, supplementary = supplementary,
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
                           subset = NULL, supplementary = NA) {
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
                           subset = NULL, supplementary = NA) {
    data
  }
)
