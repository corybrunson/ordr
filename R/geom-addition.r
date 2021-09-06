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
#' - `center`, `scale`
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
  
  default_aes = aes(
    colour = "black", alpha = NA, size = .5, linetype = 1L,
    center = 0, scale = 1
  ),
  
  setup_params = function(data, params) {
    
    if (! is.data.frame(params$new_data))
      params$new_data <- as.data.frame(params$new_data)
    
    params$type <- match.arg(params$type, c("centroid", "sequence"))
    
    params
  },
  
  setup_data = function(data, params) {
    
    # available dual dimensions
    dual_names <- intersect(names(params$new_data), data$.name_subset)
    
    # impute missing values
    params$new_data[, dual_names] <- ifelse(
      is.na(unlist(params$new_data[, dual_names, drop = TRUE])),
      0,
      unlist(params$new_data[, dual_names, drop = TRUE])
    )
    
    # data frame of individual arrow elements
    if (params$type == "sequence") {
      do.call(rbind, lapply(seq(nrow(params$new_data)), function(i) {
        row_coef <- unlist(params$new_data[i, dual_names, drop = TRUE])
        row_coef <- (row_coef - data$center) / data$scale
        row_data <- cbind(data, row = i)
        row_data[, c("x", "y")] <- row_data[, c("x", "y")] * row_coef
        row_data$x <- cumsum(row_data$x)
        row_data$y <- cumsum(row_data$y)
        row_data$xend <- dplyr::lag(row_data$x, default = 0)
        row_data$yend <- dplyr::lag(row_data$y, default = 0)
        row_data
      }))
    } else if (params$type == "centroid") {
      do.call(rbind, lapply(seq(nrow(params$new_data)), function(i) {
        n_coef <- length(dual_names)
        row_coef <- unlist(params$new_data[i, dual_names, drop = TRUE])
        row_coef <- (row_coef - data$center) / data$scale
        row_data <- cbind(data, row = i)
        row_data[, c("x", "y")] <- row_data[, c("x", "y")] * row_coef / n_coef
        row_data$xend <- 0
        row_data$yend <- 0
        cent_coef <- apply(row_data[, c("x", "y"), drop = FALSE], 2L, sum)
        row_data <- as.data.frame(lapply(row_data, only))
        row_data$xend <- cent_coef[["x"]]
        row_data$yend <- cent_coef[["y"]]
        row_data$x <- row_data$xend * n_coef
        row_data$y <- row_data$yend * n_coef
        row_data
      }))
    }
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


# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1L) {
    uniq
  } else {
    switch(
      class(x),
      integer = NA_integer_,
      numeric = NA_real_,
      character = NA_character_,
      factor = factor(NA_character_, levels = levels(x))
    )
  }
}
