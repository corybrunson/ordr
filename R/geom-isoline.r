#' @title Isolines (contour lines)
#'
#' @description `geom_isoline()` renders isolines along row or column axes.

#' @details Isolines are topographical features that separate a plot into
#'   regions in which a gradient of interest falls within a specified range.
#'   Greenacre (2010) uses them effectively to assist with the projection of
#'   markers onto axes.
#' 

#' @template ref-greenacre2010

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_isoline()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**, **`y`**
#' - `colour`
#' - `alpha`
#' - `linewidth`
#' - `linetype`
#' - `center`, `scale`
#' - `hjust`
#' - `vjust`
#' - `family`
#' - `fontface`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @param isoline_text Logical; whether to include text value marks along the
#'   isolines.
#' @inheritParams ggplot2::geom_text
#' @param by,num Intervals between elements or number of elements; specify only
#'   one.
#' @param text_dodge Numeric; the orthogonal distance of the text from the axis
#'   or isoline, as a proportion of the minimum of the plot width and height.
#' @param text.size,text.angle,text.colour,text.color,text.alpha Default
#'   aesthetics for tick mark labels. Set to NULL to inherit from the data's
#'   aesthetics.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-isoline.r
#' @export
geom_isoline <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  text_dodge = .03,
  ...,
  text.size = 3, text.angle = 0,
  text.colour = NULL, text.color = NULL, text.alpha = NULL,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIsoline,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      isoline_text = isoline_text,
      by = by, num = num,
      text_dodge = text_dodge,
      text.size = text.size,
      text.angle = text.angle,
      text.colour = text.color %||% text.colour,
      text.alpha = text.alpha,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomIsoline <- ggproto(
  "GeomIsoline", Geom,
  
  required_aes = c("x", "y"),
  non_missing_aes = c("x", "y", "angle", "radius"),
  
  default_aes = aes(
    # isoline
    colour = "black", alpha = .8,
    linewidth = .5, linetype = "dashed",
    hjust = "inward", vjust = 1,
    family = "", fontface = 1,
    # mark needs
    center = 0, scale = 1
  ),
  
  setup_params = function(data, params) {
    
    # allow only `by` or `num`, not both
    if (! is.null(params[["by"]]) && ! is.null(params[["num"]])) {
      warning("Both `by` and `num` provided; ignoring `num`.")
      params$num <- NULL
    } else if (is.null(params[["by"]]) && is.null(params[["num"]])) {
      params$num <- 6L
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    data <- ensure_cartesian_polar(data)
    
    # drop position coordinates
    data$x <- data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    isoline_text = TRUE,
    by = NULL, num = NULL,
    text_dodge = .03,
    text.size = 3, text.angle = 0,
    text.colour = NULL, text.color = NULL, text.alpha = NULL,
    parse = FALSE, check_overlap = FALSE,
    na.rm = TRUE
  ) {
    
    data <- ensure_cartesian_polar(data)
    
    # remove lengthless vectors
    data <- subset(data, x^2 + y^2 > 0)
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
    # prepare for marks
    ranges <- coord$range(panel_params)
    
    # extend isolines to just beyond window borders
    data <- delimit_rules(data, ranges$x, ranges$y)
    # calculate isoline values and positions
    data <- calibrate_rules(data, by, num, loose = FALSE)
    
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # line orientation aesthetics
    data$slope <- - data$x / data$y
    data$intercept <- data$y_t - data$slope * data$x_t
    
    # -+- ensure that vertical lines are rendered correctly -+-
    grobs <- c(grobs, list(GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )))
    
    if (isoline_text) {
      text_data <- data
      # specify independent aesthetics
      text_data$size <- text.size %||% text_data$size
      # text_data$angle <- text.angle %||% text_data$angle
      text_data$colour <- text.colour %||% text_data$colour
      text_data$alpha <- text.alpha %||% text_data$alpha
      
      # omit labels at origin
      text_data <- subset(text_data, x_t != 0 | y_t != 0)
      
      # NB: This step redefines positional aesthetics for a specific grob.
      
      # dodge axis
      text_data <- transform(
        text_data,
        x = x_t + x / radius * plot_whmin * text_dodge,
        y = y_t + y / radius * plot_whmin * text_dodge
      )
      # update text angle and put in degrees
      text_data <- transform(
        text_data,
        angle = (atan(- 1 / tan(angle)) + text.angle) * 180 / pi
      )
      
      # isoline text grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = text_data, panel_params = panel_params, coord = coord,
        parse = parse,
        check_overlap = check_overlap,
        na.rm = na.rm
      )))
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_isoline")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)
