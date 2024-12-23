#' @title Isolines (contour lines)
#'
#' @description `geom_isoline()` renders isolines along row or column axes.
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
#' - `text_colour`, `text_alpha`, `text_size`, `text_angle`,
#' - `group`
#' 

#' The prefixed aesthetics `text_*` are used by the text elements and will
#' inherit any values passed to their un-prefixed counterparts.
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
    # mark needs
    center = 0, scale = 1,
    # isoline mark text
    text_colour = "black", text_alpha = .8, text_size = 3, text_angle = 0,
    hjust = "inward", vjust = 1,
    family = "", fontface = 1
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
        angle = (atan(- 1 / tan(angle)) + text_angle) * 180 / pi
      )
      
      # specify aesthetics
      text_data$colour <- text_data$text_colour
      text_data$alpha <- text_data$text_alpha
      text_data$size <- text_data$text_size
      
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
