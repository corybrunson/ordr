#' @title Render tick marks for axes
#'
#' @description `geom_axis_ticks()` renders tick marks for specified axes among
#'   the row or column factors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis_ticks()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @import ggplot2
#' @include geom-isolines.r
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @inheritParams geom_isolines
#' @param num Integer; the number of tick marks on each axis.
#' @param tick_length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @family geom layers
#' @example inst/examples/ex-geom-axis-diabetes.r
#' @export
geom_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  num = 6L, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  num = 6L, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  num = 6L, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_axis_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols", axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  num = 6L, tick_length = .025,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxisTicks,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      num = num,
      tick_length = tick_length,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAxisTicks <- ggproto(
  "GeomAxisTicks", GeomSegment,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .25, linetype = "solid", alpha = 1
  ),
  
  setup_data = function(data, params) {
    
    # by default, render elements for all axes
    if (! is.null(params$axes)) data <- data[params$axes, , drop = FALSE]
    
    # slopes
    data$slope <- data$y / data$x
    
    # axis scales
    if (params$calibrate) {
      data <- transform(data, ss = x^2 + y^2)
      data <- transform(data, xunit = x / ss, yunit = y / ss)
      data$ss <- NULL
    } else {
      data <- transform(data, xunit = x, yunit = y)
    }
    data <- transform(data, ssunit = xunit^2 + yunit^2)
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    data$ss <- NULL
    
    # ensure intercept column (zero is appropriate for null family)
    if (params$calibrate) {
      if (! "intercept" %in% names(data)) {
        data$intercept <- 0
        if (! is.null(params$family_fun)) {
          warning("No `intercept` aesthetic provided; it has been set to zero.")
        }
      }
    } else {
      if ("intercept" %in% names(data)) {
        warning("Axis is not calibrated, so `intercept` will be ignored.")
      }
      if (! is.null(params$family_fun)) {
        warning("Axis is not calibrated, so `family_fun` will be ignored.")
      }
    }
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
    num = 6L, tick_length = .025
  ) {
    
    ranges <- coord$range(panel_params)
    if (calibrate) family_fun <- family_arg(family_fun)
    
    # window boundaries for axis positions
    data <- transform(
      data,
      winxmin = ifelse(xunit > 0, ranges$x[1], ranges$x[2]),
      winxmax = ifelse(xunit > 0, ranges$x[2], ranges$x[1]),
      winymin = ifelse(yunit > 0, ranges$y[1], ranges$y[2]),
      winymax = ifelse(yunit > 0, ranges$y[2], ranges$y[1])
    )
    
    # extreme positions, in axis units
    data <- transform(
      data,
      unitmin = (winxmin * xunit + winymin * yunit) / ssunit,
      unitmax = (winxmax * xunit + winymax * yunit) / ssunit
    )
    data$winxmin <- NULL
    data$winxmax <- NULL
    data$winymin <- NULL
    data$winymax <- NULL
    
    # calibrate axis range according to intercept and family
    if (calibrate) {
      ran_vars <- c("unitmin", "unitmax")
      data[, ran_vars] <- data[, ran_vars] + data$intercept
      if (! is.null(family_fun)) {
        data[, ran_vars] <- family_fun$linkinv(as.matrix(data[, ran_vars]))
      }
    }
    
    # element units; by default, use Wilkinson's breaks algorithm
    if (is.null(by)) {
      bys <- lapply(1:nrow(data), function(i) {
        labeling::extended(data$unitmin[i], data$unitmax[i], num)
      })
    } else {
      if (length(by) == 1) by <- rep(by, nrow(data))
      bys <- lapply(1:nrow(data), function(i) {
        floor(data$unitmin[i] / by[i]):ceiling(data$unitmax[i] / by[i]) * by[i]
      })
    }
    data <- data[rep(1:nrow(data), sapply(bys, length)), , drop = FALSE]
    data$units <- unlist(bys)
    data$unitmin <- NULL
    data$unitmax <- NULL
    # exclude ticks at origin
    data <- subset(data, units != 0)
    # text strings
    data <- transform(data, label = format(units, digits = 3))
    
    # un-calibrate axis units according to intercept and family
    if (calibrate) {
      unit_vars <- c("units")
      if (! is.null(family_fun)) {
        data[, unit_vars] <- family_fun$linkfun(as.matrix(data[, unit_vars]))
      }
      data[, unit_vars] <- data[, unit_vars] - data$intercept
    }
    
    # axis positions
    data <- transform(
      data,
      xpos = units * xunit,
      ypos = units * yunit
    )
    data$units <- NULL
    
    # tick mark radius
    rtick <- min(diff(ranges$x), diff(ranges$y)) * tick_length / 2
    # tick mark vector
    data <- transform(
      data,
      xtick = - yunit / sqrt(ssunit) * rtick,
      ytick = xunit / sqrt(ssunit) * rtick
    )
    # endpoints of tick marks
    data <- transform(
      data,
      x = xpos - xtick, xend = xpos + xtick,
      y = ypos - ytick, yend = ypos + ytick
    )
    data$xunit <- NULL
    data$yunit <- NULL
    data$ssunit <- NULL
    data$xtick <- NULL
    data$ytick <- NULL
    
    GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)
