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
#' @inheritParams ggplot2::GeomText
#' @inheritParams GeomIsolines
#' @param label_dodge Numeric; the orthogonal distance of the text from
#'   the axis, as a proportion of the minimum of the plot width and height.
#' @template param-matrix
#' @example inst/examples/diabetes-lda-supplement.r
NULL

#' @rdname geom-biplot-ticks
#' @usage NULL
#' @export
GeomAxisText <- ggproto(
  "GeomAxisText", GeomText,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    
    # ensure angles
    if (is.null(data$angle)) data$angle <- 0
    data$angle <- as.numeric(data$angle) + (180 / pi) * atan(data$y / data$x)
    
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
      if (! is.null(params$family_fun)) {
        warning("No `intercept` aesthetic provided; it has been set to zero.")
      }
    }
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    family_fun = NULL, axes = NULL, by = NULL,
    label_dodge = .025,
    parse = FALSE,
    na.rm = FALSE,
    check_overlap = FALSE
  ) {
    
    ranges <- coord$range(panel_params)
    
    # by default, render ticks for all axes
    if (! is.null(axes)) data <- data[axes, , drop = FALSE]
    family_fun <- family_arg(family_fun)
    
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
    if (! is.null(family_fun)) {
      data[, ran_vars] <- family_fun$linkinv(as.matrix(data[, ran_vars]))
    }
    
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
    # exclude ticks at origin
    data <- subset(data, units != 0)
    
    # text strings
    data <- transform(data, label = format(units, digits = 3))
    
    # positions of tick marks
    data <- transform(
      data,
      xpos = units * xunit,
      ypos = units * yunit
    )
    
    # un-transform ranges based on family
    pos_vars <- c("xpos", "ypos")
    if (! is.null(family_fun)) {
      data[, pos_vars] <- family_fun$linkfun(as.matrix(data[, pos_vars]))
    }
    data[, pos_vars] <- data[, pos_vars] - data$intercept
    
    # positions of labels
    data <- transform(
      data,
      x = xpos - yunit / sqrt(unitss) * label_dodge,
      y = ypos + xunit / sqrt(unitss) * label_dodge
    )
    
    # remove calculation steps
    data$vline <- NULL
    data$slope <- NULL
    data$xunit <- NULL
    data$yunit <- NULL
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
    
    GeomText$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      parse = parse,
      na.rm = na.rm,
      check_overlap = check_overlap
    )
  }
)

#' @rdname geom-biplot-ticks
#' @export
geom_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
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
geom_u_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
geom_v_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
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
  .matrix = "v", family_fun = NULL, axes = NULL, by = NULL,
  label_dodge = .025,
  ...,
  parse = FALSE,
  check_overlap = FALSE,
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
