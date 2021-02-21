#' @title Render tick mark labels for axes
#'
#' @description `geom_*_axis_text()` renders tick mark labels for specified
#'   axes among the row or column factors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_axis_text()` understands the following aesthetics
#' (required aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `angle`
#' - `colour`
#' - `family`
#' - `fontface`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#' - `group`
#' 

#' @name geom-biplot-ticks
#' @import ggplot2
#' @include geom-isolines.r
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_isolines
#' @param label_dodge Numeric; the orthogonal distance of the text from
#'   the axis, as a proportion of the minimum of the plot width and height.
#' @template param-matrix
#' @example inst/examples/diabetes-lda-axes.r
NULL

#' @rdname geom-biplot-ticks
#' @usage NULL
#' @export
GeomAxisText <- ggproto(
  "GeomAxisText", GeomText,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    
    # by default, render elements for all axes
    if (! is.null(params$axes)) data <- data[params$axes, , drop = FALSE]
    
    # ensure angles
    if (is.null(data$angle)) data$angle <- 0
    data$angle <- as.numeric(data$angle) + (180 / pi) * atan(data$y / data$x)
    
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
    label_dodge = .025,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
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
    
    # label positions
    data <- transform(
      data,
      x = xpos - yunit / sqrt(ssunit) * label_dodge,
      y = ypos + xunit / sqrt(ssunit) * label_dodge
    )
    data$xunit <- NULL
    data$yunit <- NULL
    data$ssunit <- NULL
    data$xpos <- NULL
    data$ypos <- NULL
    
    GeomText$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm
    )
  }
)

#' @rdname geom-biplot-ticks
#' @export
geom_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-ticks
#' @export
geom_rows_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-ticks
#' @export
geom_cols_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-ticks
#' @export
geom_biplot_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", axes = NULL, calibrate = FALSE, family_fun = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      calibrate = calibrate,
      family_fun = family_fun,
      axes = axes,
      by = by,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}
