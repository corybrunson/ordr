#' @title Render isolines for a subject or variable
#'
#' @description `geom_isolines()` renders isolines for a specified subject or
#'   variable.
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
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param axes Indices of axes for which to render elements.
#' @param calibrate Logical; whether to calibrate axis scales for inner product
#'   interpretability.
#' @param family_fun A family function, or a character string naming one, to
#'   transform the values along the axis at which to render elements.
#' @param by Interval length between elements, in the units of the ordination.
#' @family geom layers
#' @export
geom_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
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
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
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
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
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
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols", axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL,
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
      axes = axes,
      calibrate = calibrate,
      family_fun = family_fun,
      by = by,
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
    colour = "black", size = .5, linetype = "dashed", alpha = .5
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
    axes = NULL, calibrate = TRUE, family_fun = NULL, by = NULL
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
    data$ssunit <- NULL
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
    data$unitmin <- NULL
    data$unitmax <- NULL
    
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
    data$xunit <- NULL
    data$yunit <- NULL
    data$units <- NULL
    
    # line orientation aesthetics
    data <- transform(data, slope = - 1 / slope)
    data <- transform(data, intercept = ypos - slope * xpos)
    data$xpos <- NULL
    data$ypos <- NULL
    
    # -+- ensure that vertical lines are rendered correctly -+-
    GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)
