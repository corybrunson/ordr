#' Render isolines for a subject or variable
#' 

#' \code{geom_*_isolines} renders isolines for a specified subject or variable.
#' @template ggbiplot-layers

#' @section Aesthetics:

#' \code{geom_*_isolines} and \code{geom_*_ticks} understand the following
#' aesthetics (required aesthetics are in bold):

#' \itemize{
#'   \item \strong{\code{x}}
#'   \item \strong{\code{y}}
#'   \item \code{alpha}
#'   \item \code{colour}
#'   \item \code{fill}
#'   \item \code{linetype}
#'   \item \code{size}
#'   \item \code{group}
#' }
#' 

#' @name ggbiplot-isolines
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-layer
#' @param family A family function or a character string naming one, to
#'   transform the values along the axis at which to render isolines or tick
#'   marks.
#' @param ids Row indices of the subjects or variables for which isolines or
#'   tick marks will be rendered.
#' @param by Interval length between isolines or tick marks, in the units of the
#'   ordination.
#' @template param-matrix
#' @example inst/examples/ex-lm.r
#' @example inst/examples/ex-glm.r

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
    
    if (is.null(ids)) ids <- 1
    # process 'family' argument
    if (! is.null(family)) {
      if (is.character(family)) {
        family <- get(family, mode = "function", envir = parent.frame())
      }
      if (is.function(family)) {
        family <- family()
      }
    }
    
    # convert to intercepts and slopes
    data <- do.call(rbind, lapply(ids, function(i) {
      # intercept
      if (! is.null(family)) intercept <- data$intercept[i]
      # vector
      w_i <- unlist(data[i, c("x", "y")])
      # calibrated vector
      c_i <- w_i / sum(w_i ^ 2)
      # plot range of isolines along calibrated vector
      ran <- isoline_range(c_i, ranges$x, ranges$y)
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
      # slope of isolines
      m_i <- - w_i[1] / w_i[2]
      # component of final data frame from this original vector
      suppressWarnings(data.frame(
        x = k * c_i[1],
        y = k * c_i[2],
        intercept = k * c_i[2] - m_i * k * c_i[1],
        slope = m_i,
        data[i, -match(c("x", "y"), names(data))]
      ))
    }))
    
    ggplot2::GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )
  }
)

#' @rdname ggbiplot-isolines
#' @export
geom_u_isolines <- function(
  mapping = NULL, data = NULL, position = "identity",
  family = NULL, ids = 1L, by = 1,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
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
  mapping = NULL, data = NULL, position = "identity",
  family = NULL, ids = 1L, by = 1,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
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
  mapping = NULL, data = NULL, position = "identity",
  .matrix = "v", family = NULL, ids = 1L, by = 1,
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = .matrix,
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

# calculate range of isoline locations restricted to plot window
isoline_range <- function(u, xran, yran) {
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

project_onto <- function(x, y) {
  (sum(x * y) / sum(y ^ 2))
}
