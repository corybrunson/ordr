#' @title Axes through the origin
#' 
#' @description `geom_axis()` renders lines through the origin and the position
#'   of each case or variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`angle` _or_ `x,y`**
#' - `xend,yend`
#' - `x0,y0`
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
#' @example inst/examples/ex-geom-axis-glass.r
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

# TODO: Make a separate `GeomRule` ggproto class for limited axes.

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
GeomAxis <- ggproto(
  "GeomAxis", Geom,
  
  required_aes = c("x", "y"),
  optional_aes = c("xend", "yend", "x0", "y0"),
  
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
    # expect that `x,y` and `xend,yend` are collinear with the origin,
    # i.e. that the offset to `x0,y0` has not been applied;
    # this setup will offset the endpoints (if any) to inform the window
    
    # TODO: Check that `xend,yend` and `x,y` are collinear with the origin.
    
    # ensure that endpoints are not present without vectors
    if ((! is.null(data$xend) && is.null(data$x)) ||
        (! is.null(data$yend) && is.null(data$y)))
      stop("`geom_axis()` cannot accept `xend|yend` without `x|y`.")
    # axes or rules?
    use_rule <- ! is.null(data$xend) && ! is.null(data$yend)
    # offset?
    use_offset <- ! is.null(data$x0) && ! is.null(data$y0)
    
    # introduce endpoints if missing
    # NB: This will cause problems with e.g. `angle = 180/pi*atan2(x,y)`.
    # NB: This eliminates the utility of `if (is.null(data$xend))`.
    if (! use_rule) data$xend <- data$yend <- 0
    
    # introduce angles & vectors if missing
    if ((is.null(data$x) || is.null(data$y)) && 
        (is.null(data$angle) || is.null(data$radius)))
      stop("`GeomAxis` requires either `angle` and `radius` or `x` and `y`.")
    if (is.null(data$angle)) 
      data$angle <- 180 / pi * with(data, atan2(y, x))
    if (is.null(data$radius)) 
      data$radius <- sqrt( data$x ^ 2 + data$y ^ 2 )
    if (is.null(data$x)) 
      data$x <- data$xend + data$radius * cos(data$angle * pi / 180)
    if (is.null(data$y)) 
      data$y <- data$yend + data$radius * sin(data$angle * pi / 180)
    
    # axis scales and inertia
    # data <- transform(data, axis_x = x - xend, axis_y = y - yend)
    data <- transform(data, axis_x = radius * cos(angle * pi / 180))
    data <- transform(data, axis_y = radius * sin(angle * pi / 180))
    data <- transform(data, axis_ss = axis_x^2 + axis_y^2)
    # remove lengthless vectors
    data <- subset(data, axis_ss > 0)

    # # remove offset if unused
    # if (! is.null(data$x0) || ! is.null(data$y0)) {
    #   data$x0 <- data$x0 %||% 0
    #   data$y0 <- data$y0 %||% 0
    #   if (all(data$x0 == 0 & data$y0 == 0)) {
    #     data$x0 <- data$y0 <- NULL
    #   }
    # }
    # # use zero offset if missing
    # data$x0 <- data$x0 %||% 0
    # data$y0 <- data$0 %||% 0
    
    # offset endpoints
    if (use_offset) {
      data$x <- data$x + data$x0
      data$y <- data$y + data$y0
      data$xend <- data$xend + data$x0
      data$yend <- data$yend + data$y0
    }
    
    # remove position columns for whole-axis case
    # (prevent coordinates from affecting position limits)
    if (! use_rule) {
      data$xend <- NULL
      data$yend <- NULL
      data$x <- NULL
      data$y <- NULL
    }
    
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
    
    # upon `setup_data()`:
    # axis_x,axis_y = vector (1 axis measure; not a unit vector)
    # x,y = segment head
    # xend,yend = segment tail
    # x0,y0 = offset
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    # TODO: Could we obviate the need for `axis_x` & `axis_y` here?
    # # recover `x` and `y` aesthetics
    # data <- transform(
    #   data,
    #   x = axis_x, y = axis_y
    # )
    
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    # extract value ranges
    ranges <- coord$range(panel_params)
    
    # TODO: Obviate this by setting default values for these aesthetics.
    # axes or rules?
    use_rule <- ! is.null(data$xend) && ! is.null(data$yend)
    # offset?
    use_offset <- ! is.null(data$x0) && ! is.null(data$y0)
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # axis grobs: if `xend` & `yend` then segment else abline & vline
    axis_data <- unique(data)
    
    if (! use_rule) {
      
      # diagonal versus vertical lines
      axis_data <- transform(
        axis_data,
        vline = axis_x == 0 & axis_y != 0,
        slope = axis_y / axis_x,
        intercept = 0,
        xintercept = 0
      )
      if (use_offset) {
        axis_data <- transform(
          axis_data,
          # diagonal line columns
          intercept = - x0 * slope + y0,
          # vertical line columns
          xintercept =  x0 - y0 / slope
        )
      }
      
      # TODO: Move intercept and slope calculations here?
      if (any(! axis_data$vline)) {
        grobs <- c(grobs, list(GeomAbline$draw_panel(
          data = axis_data[! axis_data$vline, , drop = FALSE],
          panel_params = panel_params, coord = coord
        )))
      }
      if (any(axis_data$vline)) {
        grobs <- c(grobs, list(GeomVline$draw_panel(
          data = axis_data[axis_data$vline, , drop = FALSE],
          panel_params = panel_params, coord = coord
        )))
      }
      
    } else {
      
      grobs <- c(grobs, list(GeomSegment$draw_panel(
        data = axis_data,
        panel_params = panel_params, coord = coord
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
      
      # compute positions: if `xend` & `yend` then mid/endpoint else border
      if (! use_rule) {
        label_data <- cbind(
          label_data,
          border_points(
            label_data$axis_y / label_data$axis_x,
            panel_params$x.range, panel_params$y.range
          )
        )
      } else {
        # start with heads x,y then opt for any positions closer to the origin
        repl_end <- with(label_data, xend^2 + yend^2 < x^2 + y^2)
        label_data <- transform(
          label_data,
          x = ifelse(repl_end, xend, x),
          y = ifelse(repl_end, yend, y)
        )
        label_data <- subset(label_data, select = -c(xend, yend))
        if (use_offset) {
          repl_0 <- with(label_data, x0^2 + y0^2 < x^2 + y^2)
          label_data <- transform(
            label_data,
            x = ifelse(repl_0, x0, x),
            y = ifelse(repl_0, y0, y)
          )
          label_data <- subset(label_data, select = -c(x0, y0))
        }
      }
      
      # ensure upright angles of labels (relative to axes)
      if (is.null(label_data$angle)) label_data$angle <- 0
      label_data$angle <-
        as.numeric(label_data$angle) +
        (180 / pi) * atan(label_data$axis_y / label_data$axis_x)
      
      # dodge axis
      label_data <- transform(
        label_data,
        x = x + axis_y / sqrt(axis_ss) * plot_whmin * label_dodge,
        y = y - axis_x / sqrt(axis_ss) * plot_whmin * label_dodge
      )
      
      # axis label grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = label_data,
        # data = offset_xy(label_data),
        panel_params = panel_params, coord = coord
      )))
      
    }
    
    # TODO: Write tests that account for all possibilities:
    # * no `xend` & `yend`
    # * `xend == 0` & `yend == 0`
    # * `xend != 0` & `yend != 0`
    # compute marks (`x_val` and `y_val`):
    # if not `xend` & `yend` then first bound outside window
    if (axis_ticks || axis_text) {
      mark_data <- data
      
      # compute rule bounds in axis units
      if (use_rule) {
        # just within specified endpoints
        if (! use_offset) mark_data$x0 <- mark_data$y0 <- 0
        mark_data <- transform(
          mark_data,
          lower = ((xend - x0) * axis_x + (yend - y0) * axis_y) / sqrt(axis_ss),
          upper = ((x    - x0) * axis_x + (y    - y0) * axis_y) / sqrt(axis_ss)
        )
      } else {
        # just beyond window borders
        mark_data <- cbind(
          mark_data,
          with(mark_data, limit_values(axis_x, axis_y, ranges$x, ranges$y))
        )
      }
      
      # calculate rule values and their positions
      mark_data <- calibrate_rules(mark_data, by, num, loose = ! use_rule)
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
        data = offset_xy(tick_data),
        panel_params = panel_params, coord = coord
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
        data = offset_xy(text_data),
        panel_params = panel_params, coord = coord,
        parse = parse,
        check_overlap = check_overlap,
        na.rm = na.rm
      )))
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_axis")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)

offset_xy <- function(data) {
  if (is.null(data$x0) && is.null(data$y0)) return(data)
  if (is.null(data$x0)) data$x0 <- 0
  if (is.null(data$y0)) data$y0 <- 0
  
  # offset intercepts
  # angle <- 180 / pi * atan2(data$axis_y, data$axis_x)
  if (! is.null(data$intercept))
    # data$intercept <- data$intercept + data$yintercept
    data$intercept <- data$intercept - data$x0 * data$slope + data$y0
  if (! is.null(data$xintercept))
    # data$xintercept <- - data$xintercept * data$slope
    data$xintercept <- data$xintercept + data$x0 - data$y0 / data$slope
  
  # positional variables to offset
  offset_cols <- lapply(
    c("x", "y"),
    \(xy) c(
      paste0("axis_", xy),
      paste0(xy, c("", "end", "tick"))
    )
  ) |> 
    lapply(intersect, names(data)) |> 
    stats::setNames(c("x", "y"))
  
  # offset positional variables
  for (col in offset_cols$x) data[[col]] <- data[[col]] + data$x0
  for (col in offset_cols$y) data[[col]] <- data[[col]] + data$y0
  
  data
}
