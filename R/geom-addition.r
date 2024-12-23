#' @title Render interpolation of new rows from columns (or vice-versa)
#' 

#' @description `geom_addition()` renders a tail-to-head sequence of arrows
#'   (`type = "centroid"`) or a scaled centroid arrow (`type = "centroid"`) that
#'   represents the interpolation of a new row from its column entries to its
#'   artificial coordinates.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_addition()` understands the custom **`interpolate`** aesthetic, which
#' tells the internals which columns of `new_data` contain the variables to be
#' used for interpolation. Except in rare cases, `new_data` should contain the
#' same rows or columns as the ordinated data and `interpolate` should be set to
#' `name` (procured by [augment_ord()]).

#' `geom_addition()` additionally understands the following aesthetics (required
#' aesthetics are in bold):

#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `fill`
#' - `shape`
#' - `stroke`
#' - `point_size`
#' - `point_fill`
#' - `center`, `scale`
#' - `group`
#' 

#' @include geom-vector.r
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

#' @rdname geom_addition
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

#' @rdname geom_addition
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

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAddition <- ggproto(
  "GeomAddition", GeomVector,
  
  required_aes = c(),
  
  default_aes = aes(
    colour = "black", alpha = NA, size = .5, linetype = 1L, fill = NA,
    shape = 19L, stroke = .5, point_size = 1.5, point_fill = NA,
    center = 0, scale = 1
  ),
  
  setup_params = function(data, params) {
    
    if (! is.data.frame(params$new_data))
      params$new_data <- as.data.frame(params$new_data)
    
    # available dual dimensions
    if (is.null(data$interpolate)) {
      if (ncol(params$new_data) == nrow(data)) {
        dual_names <- names(params$new_data)
      } else {
        stop("`new_data` needs column per variable, or else named columns.")
      }
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
    na.rm = FALSE
  ) {
    
    if (! coord$is_linear()) {
      warning("Vectors are not yet tailored to non-linear coordinates.")
    }
    type <- match.arg(type, c("centroid", "sequence"))
    # reverse ends of `arrow`
    if (! is.null(arrow)) arrow$ends <- c(2L, 1L, 3L)[arrow$ends]
    
    # ???
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
      cent_data$size <- cent_data$point_size
      cent_data$fill <- cent_data$point_fill
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
    grob$name <- grid::grobName(grob, "geom_addition")
    grob
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
