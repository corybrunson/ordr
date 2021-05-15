#' @title Render isolines (contour lines) along axes
#'
#' @description `geom_isolines()` renders isolines along row or column axes.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_isolines()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' - `center` (for un-scaling)
#' - `scale` (for un-scaling)
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param subset Indices or logical vector of rows or columns for which to
#'   render elements.
#' @param by,num Intervals between elements or number of elements; specify only
#'   one.
#' @family geom layers
#' @export
geom_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      subset = subset,
      by = by, num = num,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      subset = subset,
      by = by, num = num,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      subset = subset,
      by = by, num = num,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  subset = NULL, by = NULL, num = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      subset = subset,
      by = by, num = num,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomIsolines <- ggproto(
  "GeomIsolines", GeomAbline,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", alpha = .8, size = .5, linetype = "dashed"
  ),
  
  setup_data = function(data, params) {
    
    # by default, render elements for all rows
    if (! is.null(params$subset)) data <- data[params$subset, , drop = FALSE]
    # allow only `by` or `num`, not both
    if (! is.null(params$by) && ! is.null(params$num)) {
      warning("Both `by` and `num` provided; ignoring `num`.")
      params$num <- NULL
    }
    
    # centers and scales
    # (center is position on axis at origin)
    if (! "center" %in% names(data)) data$center <- 0
    if (! "scale" %in% names(data)) data$scale <- 1
    
    # axis scales
    data <- transform(data, axis_x = x, axis_y = y)
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    subset = NULL, by = NULL, num = NULL
  ) {
    if (is.null(by) && is.null(num)) num <- 6L
    
    ranges <- coord$range(panel_params)
    
    data <- calibrate_axes(data, ranges, by, num)
    
    # line orientation aesthetics
    data$slope <- - data$axis_x / data$axis_y
    data$intercept <- data$y_val - data$slope * data$x_val
    # discard unneeded columns
    
    # -+- ensure that vertical lines are rendered correctly -+-
    grob <- GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
    grob$name <- grid::grobName(grob, "geom_isolines")
    grob
  }
)

# `data` must have fields 'axis_x' and 'axis_y'
calibrate_axes <- function(data, ranges, by, num) {
  
  # window boundaries for axis positions
  data$win_xmin <- ifelse(data$axis_x > 0, ranges$x[[1L]], ranges$x[[2L]])
  data$win_xmax <- ifelse(data$axis_x > 0, ranges$x[[2L]], ranges$x[[1L]])
  data$win_ymin <- ifelse(data$axis_y > 0, ranges$y[[1L]], ranges$y[[2L]])
  data$win_ymax <- ifelse(data$axis_y > 0, ranges$y[[2L]], ranges$y[[1L]])
  # vector lengths
  data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
  # project window corners onto axis (isoline extrema), in axis units
  data$axis_min <-
    (data$win_xmin * data$axis_x + data$win_ymin * data$axis_y) / data$axis_ss
  data$axis_max <-
    (data$win_xmax * data$axis_x + data$win_ymax * data$axis_y) / data$axis_ss
  data$win_xmin <- NULL
  data$win_xmax <- NULL
  data$win_ymin <- NULL
  data$win_ymax <- NULL
  
  # label ranges
  data$label_min <- data$center + data$scale * data$axis_min
  data$label_max <- data$center + data$scale * data$axis_max
  data$axis_min <- NULL
  data$axis_max <- NULL
  
  # element units; by default, use Wilkinson's breaks algorithm
  label_vals <- if (is.null(by)) {
    lapply(seq(nrow(data)), function(i) {
      labeling::extended(data$label_min[[i]], data$label_max[[i]], num)
    })
  } else {
    if (length(by) == 1L) by <- rep(by, nrow(data))
    lapply(seq(nrow(data)), function(i) {
      by[[i]] * seq(
        floor(data$label_min[[i]] / by[[i]]),
        ceiling(data$label_max[[i]] / by[[i]])
      )
    })
  }
  data <- data[rep(seq(nrow(data)), sapply(label_vals, length)),
               , drop = FALSE]
  data$label <- unlist(label_vals)
  data$label_min <- NULL
  data$label_max <- NULL
  
  # axis positions
  data$axis_val <- (data$label - data$center) / data$scale
  data$x_val <- data$axis_val * data$axis_x
  data$y_val <- data$axis_val * data$axis_y
  data$axis_val <- NULL
  
  data
}
