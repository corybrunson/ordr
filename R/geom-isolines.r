#' @title Render isolines for a subject or variable
#'
#' @description `geom_*_isolines()` renders isolines for a specified subject or
#'   variable.
#' @template ggbiplot-layers

#' @section Aesthetics:

#' `geom_*_isolines()` and `geom_*_ticks()` understand the following aesthetics
#' (required aesthetics are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name ggbiplot-isolines
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param family A family function or a character string naming one, to
#'   transform the values along the axis at which to render isolines or tick
#'   marks.
#' @param ids Row indices of the subjects or variables for which isolines or
#'   tick marks will be rendered.
#' @param by Interval length between isolines or tick marks, in the units of the
#'   ordination.
#' @param ticks.length Numeric; the length of the tick marks, as a proportion of
#'   the plot width.
#' @template param-matrix
#' @example inst/examples/mtcars-lm-isolines.r
#' @example inst/examples/bioenv-lm-isolines.r
#' @example inst/examples/bioenv-glm-isolines.r
NULL

#' @rdname ggbiplot-isolines
#' @usage NULL
#' @export
GeomTicks <- ggproto(
  "GeomTicks", GeomSegment,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .5, linetype = "solid", alpha = 1
  ),
  
  draw_panel = function(
    data, panel_params, coord,
    family = NULL, ids = 1L, by = 1,
    ticks.length = .05
  ) {
    if (! is.null(family)) {
      if (! "intercept" %in% names(data)) {
        data$intercept <- 0
        warning("No 'intercept' aesthetic provided; it has been set to zero.")
      }
    }
    
    ranges <- coord$range(panel_params)
    
    if (is.null(ids)) ids <- 1L
    # process 'family' argument
    family <- family_arg(family)
    
    # convert to intercepts and slopes
    data <- do.call(rbind, lapply(ids, function(i) {
      # vector
      w_i <- unname(unlist(data[i, c("x", "y")]))
      # calibrated vector
      c_i <- w_i / sum(w_i ^ 2)
      
      # intercept (`NULL` with `family`)
      intercept <- if (! is.null(family)) data$intercept[i]
      
      # plot range of isolines along calibrated vector
      ran <- axis_range(c_i, ranges$x, ranges$y)
      
      # calculate positions of tick marks
      k_i <- axis_positions(ran, family, by, intercept)
      
      # slope of tick marks
      m_i <- - w_i[1] / w_i[2]
      # unit vector in tick mark direction
      u_i <- c(-w_i[2], w_i[1]) / sum(w_i ^ 2)
      # component of final data frame from this original axis
      suppressWarnings(data.frame(
        x =    k_i * c_i[1] + u_i[1] * ticks.length * diff(ranges$y),
        xend = k_i * c_i[1] - u_i[1] * ticks.length * diff(ranges$y),
        y =    k_i * c_i[2] + u_i[2] * ticks.length * diff(ranges$y),
        yend = k_i * c_i[2] - u_i[2] * ticks.length * diff(ranges$y),
        data[i, -match(c("x", "y"), names(data))]
      ))
    }))
    
    GeomSegment$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname ggbiplot-isolines
#' @export
geom_u_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, ids = 1L, by = 1,
  ticks.length = .05,
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
      ids = ids,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}

#' @rdname ggbiplot-isolines
#' @export
geom_v_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, ids = 1L, by = 1,
  ticks.length = .05,
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
      ids = ids,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}

#' @rdname ggbiplot-isolines
#' @export
geom_biplot_ticks <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", family = NULL, ids = 1L, by = 1,
  ticks.length = .05,
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
      ids = ids,
      by = by,
      ticks.length = ticks.length,
      ...
    )
  )
}

#' @rdname ggbiplot-isolines
#' @usage NULL
#' @export
GeomIsolines <- ggproto(
  "GeomIsolines", GeomAbline,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", size = .5, linetype = "dashed", alpha = .5
  ),
  
  draw_panel = function(
    data, panel_params, coord,
    family = NULL, ids = 1L, by = 1
  ) {
    if (! is.null(family)) {
      if (! "intercept" %in% names(data)) {
        data$intercept <- 0
        warning("No 'intercept' aesthetic provided; it has been set to zero.")
      }
    }
    
    ranges <- coord$range(panel_params)
    
    if (is.null(ids)) ids <- 1L
    # process 'family' argument
    family <- family_arg(family)
    
    # convert to intercepts and slopes
    data <- do.call(rbind, lapply(ids, function(i) {
      # intercept
      #if (! is.null(family)) intercept <- data$intercept[i]
      # vector
      w_i <- unlist(data[i, c("x", "y")])
      # calibrated vector
      c_i <- w_i / sum(w_i ^ 2)
      
      # intercept (`NULL` with `family`)
      intercept <- if (! is.null(family)) data$intercept[i]
      
      # plot range of isolines along calibrated vector
      ran <- axis_range(c_i, ranges$x, ranges$y)
      
      # calculate positions of tick marks
      k_i <- axis_positions(ran, family, by, intercept)
      
      # slope of isolines
      m_i <- - w_i[1] / w_i[2]
      # component of final data frame from this original axis
      suppressWarnings(data.frame(
        x = k_i * c_i[1],
        y = k_i * c_i[2],
        intercept = k_i * c_i[2] - m_i * k_i * c_i[1],
        slope = m_i,
        data[i, -match(c("x", "y"), names(data))]
      ))
    }))
    
    GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname ggbiplot-isolines
#' @export
geom_u_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, ids = 1L, by = 1,
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
      family = family,
      ids = ids,
      by = by,
      ...
    )
  )
}

#' @rdname ggbiplot-isolines
#' @export
geom_v_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  family = NULL, ids = 1L, by = 1,
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
      family = family,
      ids = ids,
      by = by,
      ...
    )
  )
}

#' @rdname ggbiplot-isolines
#' @export
geom_biplot_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v", family = NULL, ids = 1L, by = 1,
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
      family = family,
      ids = ids,
      by = by,
      ...
    )
  )
}

family_arg <- function(family) {
  if (! is.null(family)) {
    if (is.character(family)) {
      family <- get(family, mode = "function", envir = parent.frame())
    }
    if (is.function(family)) {
      family <- family()
    }
  }
  family
}

# calculate range of isoline locations restricted to plot window
axis_range <- function(u, xran, yran) {
  m <- u[2] / u[1]
  ran <- if (m > 0) {
    c(project_onto(c(xran[1], yran[1]), u),
      project_onto(c(xran[2], yran[2]), u))
  } else if (m < 0) {
    c(project_onto(c(xran[1], yran[2]), u),
      project_onto(c(xran[2], yran[1]), u))
  } else if (m == 0) {
    c(project_onto(c(xran[1], 0), u),
      project_onto(c(xran[2], 0), u))
  } else if (is.infinite(m)) {
    c(project_onto(c(0, yran[1]), u),
      project_onto(c(0, yran[2]), u))
  }
  ran
}

axis_positions <- function(ran, family, by, intercept = NULL) {
  if (! is.null(family)) {
    # shift by intercept
    ran <- ran + intercept
    # un-linked range of isolines via inverse link function
    ran <- family$linkinv(ran)
  }
  # range of `by`-multiples of calibrated vector within plot range
  k_ran <- c(ceiling(min(ran) / by), floor(max(ran) / by))
  # positions of isolines
  k <- seq(k_ran[1] * by, k_ran[2] * by, by = by)
  if (! is.null(family)) {
    # re-link positions of isolines via link function
    k <- family$linkfun(k)
    # un-shift by intercept
    k <- k - intercept
  }
  k
}

project_onto <- function(x, y) {
  (sum(x * y) / sum(y ^ 2))
}
