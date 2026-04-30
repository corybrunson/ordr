#' @title Render plot elements for one matrix of an ordination
#'
#' @description These stats merely tell [ggplot2::ggplot()] which factor of an
#'   ordination to pull data from for a plot layer. They are invoked internally
#'   by the various [`geom_*_*()`][biplot-geoms] layers.
#'
#'   The helper functions `rows_stat()` and `cols_stat()` convert statistical
#'   transformation ggprotos, e.g. `StatIdentity`, or their character string
#'   desingations, e.g. `"identity"`, to character string designations for
#'   corresponding factor-specific ggprotos. Note that these ggprotos may not
#'   exist, in which case the error would only appear when the plot is
#'   assembled.
#'
#'   The helper functions `rows_data()` and `cols_data()` return ordination
#'   subsetting functions that apply `subset` and `elements` to the designated
#'   matrix factor. They are designed to be used in standard `stat_*()`
#'   constructor calls to apply the transformation to one matrix factor.
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
  mapping = NULL, data = NULL,
  geom = "point", position = "identity",
  subset = NULL, elements = "active",
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
  mapping = NULL, data = NULL,
  geom = "axis", position = "identity",
  subset = NULL, elements = "active",
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
                           subset = NULL, elements = "active") {
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
                           subset = NULL, elements = "active") {
    data
  }
)

#' @rdname stat_rows
#' @export
rows_data <- function(subset = NULL, elements = "active") {
  function(data) {
    res <- setup_rows_data(data, list(subset = subset, elements = elements))
    res$.matrix <- "rows"
    res
  }
}

#' @rdname stat_rows
#' @export
cols_data <- function(subset = NULL, elements = "active") {
  function(data) {
    res <- setup_cols_data(data, list(subset = subset, elements = elements))
    res$.matrix <- "cols"
    res
  }
}

#' @rdname stat_rows
#' @export
rows_stat <- function(stat = StatIdentity) matrix_stat("rows", stat = stat)

#' @rdname stat_rows
#' @export
cols_stat <- function(stat = StatIdentity) matrix_stat("cols", stat = stat)
