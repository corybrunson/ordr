#' @title Render interpolation of new rows from columns (or vice-versa)
#' 

#' @description `geom_addition()` renders a tail-to-head sequence of arrows
#'   (`type = "centroid"`) or a scaled centroid arrow (`type = "centroid"`) that
#'   represents the interpolation of a new row from its column entries to its
#'   artificial coordinates.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_addition()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams geom_vector
#' @template param-geom
#' @param new_data A list (best structured as a [data.frame][base::data.frame])
#'   of row (`geom_cols_addition()`) or column (`geom_rows_addition()`) values
#'   to interpolate.
#' @param type Character value matched to `"centroid"` or `"sequence"`; the type
#'   of operations used to visualize interpolation.
#' @family geom layers
#' @export
geom_addition <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  new_data = NULL, type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAddition,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      new_data = new_data,
      type = type,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_addition <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  new_data = NULL, type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAddition,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      new_data = new_data,
      type = type,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_addition <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  new_data = NULL, type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAddition,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      new_data = new_data,
      type = type,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_addition <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  new_data = NULL, type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAddition,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      new_data = new_data,
      type = type,
      arrow = arrow,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAddition <- ggproto(
  "GeomAddition", GeomVector,
  
  required_aes = c(),
  
  setup_params = function(data, params) {
    
    if (! is.data.frame(params$new_data))
      params$new_data <- as.data.frame(params$new_data)
    
    params$type <- match.arg(params$type, c("centroid", "sequence"))
    
    params
  },
  
  setup_data = function(data, params) {
    
    # non-positional aesthetics of data
    xy_aes <- as.vector(outer(
      c("x", "y"),
      c("", "end", "max", "min", "intercept"),
      paste0
    ))
    aes_data <- data[, setdiff(names(data), xy_aes), drop = FALSE]
    
    # data frame of individual arrow elements
    add_data <- rows_to_additions(data, params$new_data, params$type)
    
    merge(add_data, aes_data, by = ".name_subset")
  },
  
  draw_panel = function(
    data, panel_params, coord,
    new_data = NULL, type = c("centroid", "sequence"),
    arrow = default_arrow,
    lineend = "round", linejoin = "mitre",
    na.rm = FALSE
  ) {
    if (! coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    type <- match.arg(type, c("centroid", "sequence"))
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      na.rm = na.rm
    )
  }
)

rows_to_additions <- function(data, new_data, type) {
  for (i in seq(nrow(new_data))) {
    new_row <- new_data[i, data$.name_subset, drop = FALSE]
    
  }
}
