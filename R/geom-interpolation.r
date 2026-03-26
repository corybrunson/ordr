#' @title Render interpolation of new rows from columns (or vice-versa)
#' 
#' @description `geom_interpolation()` renders a geometric construction that
#'   interpolates a new data matrix (row or column) element from its entries to
#'   its artificial coordinates.

#' @details Interpolation answers the following question: Given a new data
#'   element that might have appeared as a row (respectively, column) in the
#'   singular-value-decomposed data matrix, where should we expect the marker
#'   for this element to appear in the biplot? The solution is the vector sum of
#'   the column (row) units weighted by their values in the new row (column).
#'   Gower, Gardner--Lubbe, & le Roux (2011) provide two visualizations of this
#'   calculation: a tail-to-head sequence of weighted units (`type =
#'   "sequence"`), and a centroid of the weighted units scaled by the number of
#'   units (`type = "centroid"`).
#'
#'   **WARNING:**
#'   This layer is appropriate only with axes in standard coordinates (usually
#'   [`confer_inertia(p = "rows")`][confer_inertia]) and interpolative
#'   calibration ([`ggbiplot(axis.type = "interpolative")`][ggbiplot]).
#' 

#' @template ref-gower2011

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_interpolation()` requires the custom **`interpolate`** aesthetic, which
#' tells the internals which columns of the `new_data` parameter contain the
#' variables to be used for interpolation. Except in rare cases, `new_data`
#' should contain the same rows or columns as the ordinated data and
#' `interpolate` should be set to `name` (procured by [augment_ord()]).

#' `geom_interpolation()` additionally understands the following aesthetics
#' (required aesthetics are in bold):

#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `fill`
#' - `shape`
#' - `stroke`
#' - `center`, `scale`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams gggda::geom_vector
#' @template param-geom
#' @param new_data A list (best structured as a [data.frame][base::data.frame])
#'   of row (`geom_cols_interpolation()`) or column
#'   (`geom_rows_interpolation()`) values to interpolate.
#' @param type Character value matched to `"centroid"` or `"sequence"`; the type
#'   of operations used to visualize interpolation.
#' @param point.fill Default aesthetics for markers. Set to NULL to inherit from
#'   the data's aesthetics.
#' @family geom layers
#' @example inst/examples/ex-geom-interpolation.r
#' @export
geom_interpolation <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  new_data = NULL, type = c("centroid", "sequence"),
  arrow = default_arrow,
  ...,
  point.fill = NA,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomInterpolation,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      new_data = new_data,
      type = type,
      point.fill = point.fill,
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
GeomInterpolation <- ggproto(
  "GeomInterpolation", GeomSegment,
  
  required_aes = c("interpolate"),
  optional_aes = c("center", "scale"),
  non_missing_aes = c(
    "x", "y", "xend", "yend",
    "size", "linetype", "linewidth"
  ),
  
  default_aes = aes(
    linewidth = 0.5, linetype = 1L, size = 1.5,
    colour = "black", fill = NA, alpha = NA,
    shape = 19L, stroke = .5,
    center = 0, scale = 1, interpolate = NULL
  ),
  
  setup_params = function(data, params) {
    
    # coerce only if necessary; preserve tibble class
    if (! is.data.frame(params$new_data))
      params$new_data <- as.data.frame(params$new_data)
    
    # available dual dimensions
    if (is.null(data$interpolate)) {
      stop("The `interpolate` aesthetic must match a column of `new_data`.")
    } else {
      dual_names <- data$interpolate
      lost_names <- setdiff(dual_names, names(params$new_data))
      if (length(lost_names) > 0L)
        params$new_data[, lost_names] <- NA_real_
      params$new_data <- params$new_data[, dual_names]
    }
    
    # impute missing values
    params$new_data[, dual_names] <- ifelse(
      is.na(unlist(params$new_data[, dual_names, drop = TRUE])),
      if (is.null(data$center)) 0 else
        rep(data$center, each = nrow(params$new_data)),
      unlist(params$new_data[, dual_names, drop = TRUE])
    )
    
    params$type <- match.arg(params$type, c("centroid", "sequence"))
    
    params
  },
  
  setup_data = function(data, params) data,
  
  draw_panel = function(
    data, panel_params, coord,
    new_data = NULL, type = c("centroid", "sequence"),
    arrow = default_arrow, lineend = "round", linejoin = "mitre",
    rule = "evenodd",
    point.fill = NA,
    na.rm = FALSE
  ) {
    
    if (! coord$is_linear()) {
      warning("Interpolation is not yet tailored to non-linear coordinates.")
      rlang::warn(
        "Interpolation is not yet tailored to non-linear coordinates.",
        .frequency = "regularly",
        .frequency_id = "GeomInterpolation$draw_panel-is_linear"
      )
    }
    type <- match.arg(type, c("centroid", "sequence"))
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    # names restricted to within `$setup_params()`
    dual_names <- names(new_data)
    n_coef <- length(dual_names)
    
    # data frames of individual arrow elements and convex hulls
    if (type == "sequence") {
      hull_data <- NULL
      cent_data <- NULL
      vec_data <- do.call(rbind, lapply(seq(nrow(new_data)), function(i) {
        row_coef <- unlist(new_data[i, dual_names, drop = TRUE])
        row_coef <- (row_coef - data$center) / data$scale
        row_data <- cbind(data, row = i)
        row_data[, c("x", "y")] <- row_data[, c("x", "y")] * row_coef
        row_data$x <- cumsum(row_data$x)
        row_data$y <- cumsum(row_data$y)
        row_data$xend <- dplyr::lag(row_data$x, default = 0)
        row_data$yend <- dplyr::lag(row_data$y, default = 0)
        row_data
      }))
    } else if (type == "centroid") {
      # keep only columns that are constant throughout the data
      only_data <- dplyr::select_if(data, is_const)[1L, , drop = FALSE]
      only_data$x <- NULL
      only_data$y <- NULL
      hull_data <- do.call(rbind, lapply(seq(nrow(new_data)), function(i) {
        row_coef <- unlist(new_data[i, dual_names, drop = TRUE])
        row_coef <- (row_coef - data$center) / data$scale
        row_data <- cbind(data[, c("x", "y"), drop = FALSE], row = i)
        row_data[, c("x", "y")] <- row_data[, c("x", "y")] * row_coef
        chull_ids <- chull(row_data)
        row_data[chull_ids[c(seq_along(chull_ids), 1L)], , drop = FALSE]
      }))
      hull_data <- cbind(hull_data, only_data, row.names = NULL)
      hull_data$group <- hull_data$row
      cent_data <- do.call(rbind, lapply(seq(nrow(new_data)), function(i) {
        row_coef <- unlist(new_data[i, dual_names, drop = TRUE])
        row_coef <- (row_coef - data$center) / data$scale
        cent_coef <- apply(
          as.matrix(data[, c("x", "y"), drop = FALSE]) * row_coef / n_coef,
          2L, sum
        )
        data.frame(x = cent_coef[["x"]], y = cent_coef[["y"]])
      }))
      cent_data <- cbind(cent_data, only_data, row.names = NULL)
      vec_data <- transform(
        cent_data,
        xend = 0, yend = 0, x = x * n_coef, y = y * n_coef
      )
      # specify independent aesthetics
      cent_data$fill <- point.fill %||% cent_data$fill
      cent_data$linetype <- NULL
    }
    
    # list of grobs
    grobs <- list()
    # convex hull of summand vectors
    if (! is.null(hull_data)) {
      grobs <- c(grobs, list(GeomPolygon$draw_panel(
        data = hull_data, panel_params = panel_params, coord = coord,
        rule = rule
      )))
    }
    # centroids of summand vectors
    if (! is.null(cent_data)) {
      grobs <- c(grobs, list(GeomPoint$draw_panel(
        data = cent_data, panel_params = panel_params, coord = coord,
        na.rm = na.rm
      )))
    }
    # vectors
    grobs <- c(grobs, list(GeomSegment$draw_panel(
      data = vec_data, panel_params = panel_params, coord = coord,
      arrow = arrow, lineend = lineend, linejoin = linejoin,
      na.rm = na.rm
    )))
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_interpolation")
    grob
  },
  
  rename_size = FALSE
)
