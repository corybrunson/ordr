#' @title Render tick marks for axes
#'
#' @description `geom_*_ticks()` renders tick marks for specified axes among the
#'   row or column factors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_ticks()` understands the following aesthetics
#' (required aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-ticks
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param family A family function or a character string naming one, to
#'   transform the values along the axis at which to render tick marks.
#' @param axes Indices for which tick marks will be rendered.
#' @param by Interval length between tick marks, in the units of the ordination.
#' @param ticks.length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @template param-matrix
#' @example inst/examples/diabetes-lda-supplement.r
NULL

#' @rdname geom-biplot-ticks
#' @usage NULL
#' @export
GeomTicks <- ggproto(
  "GeomTicks", GeomSegment,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .5, linetype = "solid", alpha = 1
  ),
  
  setup_data = function(data, params) {
    
    # diagonal versus vertical lines
    data$vline <- data$x == 0 & data$y != 0
    # tick slopes
    data$slope <- data$y / data$x
    # axis scales
    data$xunit <- data$x
    data$yunit <- data$y
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    # ensure intercept column (zero is appropriate for null family)
    if (! "intercept" %in% names(data)) {
      data$intercept <- 0
      if (! is.null(params$family)) {
        warning("No `intercept` aesthetic provided; it has been set to zero.")
      }
    }
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    family = NULL, axes = NULL, by = NULL,
    ticks.length = .025
  ) {
    
    ranges <- coord$range(panel_params)
    
    # by default, render ticks for all axes
    if (! is.null(axes)) data <- data[axes, , drop = FALSE]
    # process 'family' argument
    family <- family_arg(family)
    
    # window boundaries for axis ticks
    data <- transform(
      data,
      # sum of squares of axis unit
      unitss = xunit^2 + yunit^2,
      winxmin = ifelse(xunit > 0, ranges$x[1], ranges$x[2]),
      winxmax = ifelse(xunit > 0, ranges$x[2], ranges$x[1]),
      winymin = ifelse(yunit > 0, ranges$y[1], ranges$y[2]),
      winymax = ifelse(yunit > 0, ranges$y[2], ranges$y[1])
    )
    # extreme positions of ticks, in terms of axis unit
    data <- transform(
      data,
      unitmin = (winxmin * xunit + winymin * yunit) / unitss,
      unitmax = (winxmax * xunit + winymax * yunit) / unitss
    )
    
    # transform ranges based on family
    ran_vars <- c("winxmin", "winxmax", "winymin", "winymax")
    data[, ran_vars] <- data[, ran_vars] + data$intercept
    if (! is.null(family)) data[, ran_vars] <- family$linkinv(data[, ran_vars])
    
    # by default, use Wilkinson's breaks algorithm
    if (is.null(by)) {
      bys <- lapply(1:nrow(data), function(i) {
        labeling::extended(data$unitmin[i], data$unitmax[i], 6)
      })
    } else {
      if (length(by) == 1) by <- rep(by, nrow(data))
      bys <- lapply(1:nrow(data), function(i) {
        floor(data$unitmin[i] / by[i]):ceiling(data$unitmax[i] / by[i]) * by[i]
      })
    }
    data <- data[rep(1:nrow(data), sapply(bys, length)), , drop = FALSE]
    data$units <- unlist(bys)
    # positions of tick marks
    data <- transform(
      data,
      xpos = units * xunit,
      ypos = units * yunit
    )
    
    # un-transform ranges based on family
    pos_vars <- c("xpos", "ypos")
    if (! is.null(family)) data[, pos_vars] <- family$linkfun(data[, pos_vars])
    data[, pos_vars] <- data[, pos_vars] - data$intercept
    
    # tick mark radius
    rtick <- min(diff(ranges$x), diff(ranges$y)) * ticks.length / 2
    # tick mark vector
    data <- transform(
      data,
      xtick = - yunit / sqrt(unitss) * rtick,
      ytick = xunit / sqrt(unitss) * rtick
    )
    # endpoints of tick marks
    data <- transform(
      data,
      x = xpos - xtick, xend = xpos + xtick,
      y = ypos - ytick, yend = ypos + ytick
    )
    
    # remove calculation steps
    data$vline <- NULL
    data$slope <- NULL
    data$xunit <- NULL
    data$yunit <- NULL
    data$xtick <- NULL
    data$ytick <- NULL
    data$unitss <- NULL
    data$winxmin <- NULL
    data$winxmax <- NULL
    data$winymin <- NULL
    data$winymax <- NULL
    data$unitmin <- NULL
    data$unitmax <- NULL
    data$units <- NULL
    data$xpos <- NULL
    data$ypos <- NULL
    
    # -+- NEED TO ADD TICK LABELS -+-
    
    GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname geom-biplot-ticks
#' @export
geom_u_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, axes = NULL, by = NULL,
  ticks.length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      family = family,
      axes = axes,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}

#' @rdname geom-biplot-ticks
#' @export
geom_v_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, axes = NULL, by = NULL,
  ticks.length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      family = family,
      axes = axes,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}

#' @rdname geom-biplot-ticks
#' @export
geom_biplot_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", family = NULL, axes = NULL, by = NULL,
  ticks.length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      family = family,
      axes = axes,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}
