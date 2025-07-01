#' @title Isolines (contour lines)
#'
#' @description `geom_isoline()` renders isolines along row or column axes.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_isoline()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `colour`
#' - `alpha`
#' - `linewidth`
#' - `linetype`
#' - `center`, `scale`
#' - `angle`
#' - `hjust`
#' - `vjust`
#' - `family`
#' - `fontface`
#' - `text_colour`, `text_alpha`, `text_size`,
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
#' @param label_dodge Numeric; the orthogonal distance of the text from the axis
#'   or isoline, as a proportion of the minimum of the plot width and height.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-isoline-diabetes.r
#' @export
geom_isoline <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  label_dodge = .03,
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
      label_dodge = label_dodge,
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
  
  default_aes = aes(
    # isoline
    colour = "black", alpha = .8,
    linewidth = .5, linetype = "dashed",
    # mark needs
    center = 0, scale = 1,
    # isoline mark text
    text_colour = "black", text_alpha = .8, text_size = 3,
    angle = 0,
    hjust = 0.5, vjust = 0.5,
    family = "", fontface = 1
  ),
  
  setup_params = function(data, params) {
    
    # allow only `by` or `num`, not both
    if (! is.null(params$by) && ! is.null(params$num)) {
      warning("Both `by` and `num` provided; ignoring `num`.")
      params$num <- NULL
    }
    
    params
  },
  
  setup_data = function(data, params) {
    
    # centers and scales
    # (center is position on axis at origin)
    #if (! "center" %in% names(data)) data$center <- 0
    #if (! "scale" %in% names(data)) data$scale <- 1
    
    # axis scales
    data <- transform(data, axis_x = x, axis_y = y)
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    isoline_text = TRUE,
    by = NULL, num = NULL,
    label_dodge = .03,
    parse = FALSE, check_overlap = FALSE,
    na.rm = TRUE
  ) {
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
    # prepare for marks
    ranges <- coord$range(panel_params)
    data <- calibrate_axes(data, ranges, by, num)
    
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # line orientation aesthetics
    data$slope <- - data$axis_x / data$axis_y
    data$intercept <- data$y_val - data$slope * data$x_val
    
    # -+- ensure that vertical lines are rendered correctly -+-
    grobs <- c(grobs, list(GeomAbline$draw_panel(
      data = data, panel_params = panel_params, coord = coord
    )))
    
    if (isoline_text) {
      text_data <- data
      data$slope <- NULL
      data$intercept <- NULL
      
      # specify aesthetics
      text_data$colour <- text_data$text_colour
      text_data$alpha <- text_data$text_alpha
      text_data$size <- text_data$text_size
      
      # omit labels at origin
      text_data <-
        text_data[text_data$x != 0 | text_data$y != 0, , drop = FALSE]
      # calculate angles
      if (is.null(text_data$angle)) text_data$angle <- 0
      text_data$angle <-
        as.numeric(text_data$angle) +
        atan(- text_data$axis_x / text_data$axis_y) / pi * 180
      # dodge axis
      text_data <- transform(
        text_data,
        x = x_val + axis_x / sqrt(axis_ss) * plot_whmin * label_dodge,
        y = y_val + axis_y / sqrt(axis_ss) * plot_whmin * label_dodge
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
