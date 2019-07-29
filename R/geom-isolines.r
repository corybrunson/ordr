#' @title Render isolines for a subject or variable
#'
#' @description `geom_*_isolines()` renders isolines for a specified subject or
#'   variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_isolines()` understands the following aesthetics
#' (required aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-isolines
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param family A family function or a character string naming one, to
#'   transform the values along the axis at which to render isolines.
#' @param axes Indices for which isolines will be rendered.
#' @param by Interval length between isolines, in the units of the ordination.
#' @template param-matrix
#' @example inst/examples/mtcars-lm-isolines.r
#' @example inst/examples/bioenv-lm-isolines.r
#' @example inst/examples/bioenv-glm-isolines.r
NULL

#' @rdname geom-biplot-isolines
#' @usage NULL
#' @export
GeomIsolines <- ggproto(
  "GeomIsolines", GeomAbline,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .5, linetype = "dashed", alpha = .5
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
      if (! is.null(params$family_fun)) {
        warning("No `intercept` aesthetic provided; it has been set to zero.")
      }
    }
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    family_fun = NULL, axes = NULL, by = NULL
  ) {
    if (! is.null(family_fun)) {
      if (! "intercept" %in% names(data)) {
        data$intercept <- 0
        warning("No 'intercept' aesthetic provided; it has been set to zero.")
      }
    }
    
    ranges <- coord$range(panel_params)
    
    if (is.null(axes)) axes <- 1L
    family_fun <- family_arg(family_fun)
    
    # window boundaries for axis positions
    data <- transform(
      data,
      # sum of squares of axis unit
      unitss = xunit^2 + yunit^2,
      winxmin = ifelse(xunit > 0, ranges$x[1], ranges$x[2]),
      winxmax = ifelse(xunit > 0, ranges$x[2], ranges$x[1]),
      winymin = ifelse(yunit > 0, ranges$y[1], ranges$y[2]),
      winymax = ifelse(yunit > 0, ranges$y[2], ranges$y[1])
    )
    # extreme positions, in terms of axis unit
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
    
    # axis positions
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
    
    # abline orientation aesthetics
    data <- transform(data, slope = -1 / slope)
    data <- transform(data, intercept = ypos - slope * xpos)
    
    # remove calculation steps
    data$vline <- NULL
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
    
    GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname geom-biplot-isolines
#' @export
geom_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
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
      family_fun = family_fun,
      axes = axes,
      by = by,
      ...
    )
  )
}

#' @rdname geom-biplot-isolines
#' @export
geom_u_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      family_fun = family_fun,
      axes = axes,
      by = by,
      ...
    )
  )
}

#' @rdname geom-biplot-isolines
#' @export
geom_v_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family_fun = NULL, axes = NULL, by = NULL,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomIsolines,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      family_fun = family_fun,
      axes = axes,
      by = by,
      ...
    )
  )
}

#' @rdname geom-biplot-isolines
#' @export
geom_biplot_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", family_fun = NULL, axes = NULL, by = NULL,
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
      family_fun = family_fun,
      axes = axes,
      by = by,
      ...
    )
  )
}
