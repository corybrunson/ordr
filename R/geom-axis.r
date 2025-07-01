#' @title Axes through the origin
#' 
#' @description `geom_axis()` renders lines through the origin and the position
#'   of each case or variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`x`**
#' - **`y`**
#' - `colour`
#' - `alpha`
#' - `linewidth`
#' - `linetype`
#' - `label`
#' - `center`, `scale`
#' - `label_colour`, `label_alpha`, `label_size`, `label_angle`,
#'   `label_hjust`, `label_vjust`, `label_family`, `label_fontface`
#' - `tick_colour`, `tick_alpha`, `tick_linewidth`, `tick_linetype`
#' - `text_colour`, `text_alpha`, `text_size`, `text_angle`,
#'   `text_hjust`, `text_vjust`, `text_family`, `text_fontface`
#' - `group`
#' 

#' The prefixed aesthetics `label_*`, `tick_*`, and `text_*` are used by the
#' text elements and will inherit any values passed to their un-prefixed
#' counterparts, if recognized.
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_isoline
#' @template param-geom
#' @param axis_labels,axis_ticks,axis_text Logical; whether to include labels,
#'   tick marks, and text value marks along the axes.
#' @param tick_length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @param text_dodge Numeric; the orthogonal distance of the text from the axis,
#'   as a proportion of the minimum of the plot width and height.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-axis-diabetes.r
#' @export
geom_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
  by = NULL, num = NULL,
  tick_length = .025, text_dodge = .03, label_dodge = .03,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      axis_labels = axis_labels, axis_ticks = axis_ticks, axis_text = axis_text,
      by = by, num = num,
      tick_length = tick_length,
      text_dodge = text_dodge,
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
GeomAxis <- ggproto(
  "GeomAxis", Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = aes(
    # axis
    colour = "black", alpha = NA,
    linewidth = .25, linetype = "solid",
    # axis label
    label = "",
    label_colour = "black", label_alpha = NA,
    label_size = 3.88, label_angle = 0,
    label_hjust = "inward", label_vjust = "inward",
    label_family = "", label_fontface = 1,
    # mark needs
    center = 0, scale = 1,
    # tick marks
    tick_colour = "black", tick_alpha = NA,
    tick_linewidth = .25, tick_linetype = "solid",
    # tick mark text
    text_colour = "black", text_alpha = NA,
    text_size = 2.6, text_angle = 0,
    text_hjust = 0.5, text_vjust = 0.5,
    text_family = "", text_fontface = 1
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
    
    # diagonal versus vertical lines
    data$vline <- data$x == 0 & data$y != 0
    # diagonal line columns
    data$intercept <- rep(0, nrow(data))
    data$slope <- data$y / data$x
    # vertical line columns
    data$xintercept <- rep(0, nrow(data))
    
    # centers and scales
    # (center is position on axis at origin)
    #if (! "center" %in% names(data)) data$center <- 0
    #if (! "scale" %in% names(data)) data$scale <- 1
    # axis scales
    data <- transform(data, axis_x = x, axis_y = y)
    # vector lengths
    data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
    
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
    by = NULL, num = NULL,
    tick_length = .025, text_dodge = .03, label_dodge = .03,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
    if (axis_ticks || axis_text) {
      # prepare for marks
      ranges <- coord$range(panel_params)
      mark_data <- calibrate_axes(data, ranges, by, num)
    }
    
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # axis grobs: combination of line grobs
    if (any(! data$vline)) {
      grobs <- c(grobs, list(GeomAbline$draw_panel(
        data = unique(data[! data$vline, , drop = FALSE]),
        panel_params = panel_params, coord = coord
      )))
    }
    if (any(data$vline)) {
      grobs <- c(grobs, list(GeomVline$draw_panel(
        data = unique(data[data$vline, , drop = FALSE]),
        panel_params = panel_params, coord = coord
      )))
    }
    
    if (axis_ticks) {
      tick_data <- mark_data
      
      # specify aesthetics
      tick_data$colour <- tick_data$tick_colour
      tick_data$alpha <- tick_data$tick_alpha
      tick_data$size <- tick_data$tick_linewidth
      tick_data$linewidth <- tick_data$tick_linewidth
      tick_data$linetype <- tick_data$tick_linetype
      
      # tick mark radius
      rtick <- plot_whmin * tick_length / 2
      # tick mark vector
      tick_data <- transform(
        tick_data,
        xtick = - axis_y / sqrt(axis_ss) * rtick,
        ytick = axis_x / sqrt(axis_ss) * rtick
      )
      # endpoints of tick marks
      tick_data <- transform(
        tick_data,
        x = x_val - xtick, xend = x_val + xtick,
        y = y_val - ytick, yend = y_val + ytick
      )
      
      # tick mark grobs
      grobs <- c(grobs, list(GeomSegment$draw_panel(
        data = tick_data, panel_params = panel_params, coord = coord
      )))
      
    }
    
    if (axis_text) {
      text_data <- mark_data
      
      # specify aesthetics
      text_data$colour <- text_data$text_colour
      text_data$alpha <- text_data$text_alpha
      text_data$size <- text_data$text_size
      text_data$angle <- text_data$text_angle
      text_data$hjust <- text_data$text_hjust
      text_data$vjust <- text_data$text_vjust
      text_data$family <- text_data$text_family
      text_data$fontface <- text_data$text_fontface
      
      # omit labels at origin
      text_data <-
        text_data[text_data$x_val != 0 | text_data$y_val != 0, , drop = FALSE]
      # calculate angles
      if (is.null(text_data$angle)) text_data$angle <- 0
      text_data$angle <-
        as.numeric(text_data$angle) +
        atan(text_data$axis_y / text_data$axis_x) / pi * 180
      # dodge axis
      text_data <- transform(
        text_data,
        x = x_val - axis_y / sqrt(axis_ss) * plot_whmin * text_dodge,
        y = y_val + axis_x / sqrt(axis_ss) * plot_whmin * text_dodge
      )
      
      # mark text grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = text_data, panel_params = panel_params, coord = coord,
        parse = parse,
        check_overlap = check_overlap,
        na.rm = na.rm
      )))
      
    }
    
    if (axis_labels) {
      label_data <- data
      
      # specify aesthetics
      label_data$colour <- label_data$label_colour
      label_data$alpha <- label_data$label_alpha
      label_data$size <- label_data$label_size
      label_data$angle <- label_data$label_angle
      label_data$hjust <- label_data$label_hjust
      label_data$vjust <- label_data$label_vjust
      label_data$family <- label_data$label_family
      label_data$fontface <- label_data$label_fontface
      
      # compute label positions
      label_data <- cbind(label_data, boundary_points(
        label_data$slope,
        panel_params$x.range, panel_params$y.range
      ))
      # ensure angles of labels
      if (is.null(label_data$angle)) label_data$angle <- 0
      label_data$angle <-
        as.numeric(label_data$angle) +
        (180 / pi) * atan(label_data$y / label_data$x)
      
      # dodge axis
      label_data <- transform(
        label_data,
        x = x + axis_y / sqrt(axis_ss) * plot_whmin * label_dodge,
        y = y - axis_x / sqrt(axis_ss) * plot_whmin * label_dodge
      )
      
      # axis label grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = label_data,
        panel_params = panel_params, coord = coord
      )))
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_axis")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)
