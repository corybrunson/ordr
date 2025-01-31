#' @title Axes through or offset from the origin
#'
#' @description `geom_axis()` renders lines through or orthogonally translated
#'   from the origin and the position of each case or variable.

#' @details Axes are lines that track the values of linear variables across a
#'   plot. Multivariate scatterplots may include more axes than plotting
#'   dimensions, in which case the plot may display only a fraction of the total
#'   variation in the data.
#'
#'   Gower & Hand (1996) recommend using axes to represent numerical variables
#'   in biplots. Consequently, Gardner & le Roux (2002) refer to these as Gower
#'   biplots.
#'
#'   Axes positioned orthogonally at the origin are a ubiquitous feature of
#'   scatterplots and used both to recover variable values from case markers
#'   (prediction) and to position new case markers from variables
#'   (interpolation). When they are not orthogonal, these two uses conflict, so
#'   interpolative versus predictive axes must be used appropriately; see
#'   [ggbiplot()].
#' 

#' @template ref-gower1996
#' @template ref-gardner2002

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`x`**
#' - **`y`**
#' - `lower`
#' - `upper`
#' - `yintercept` _or_ `xintercept` _or_ `xend` and `yend`
#' - `linetype`
#' - `linewidth`
#' - `size`
#' - `hjust`
#' - `vjust`
#' - `colour`
#' - `alpha`
#' - `label`
#' - `family`
#' - `fontface`
#' - `center`, `scale`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_isoline
#' @template param-geom
#' @param axis_labels,axis_ticks,axis_text Logical; whether to include labels,
#'   tick marks, and text value marks along the axes.
#' @param label_dodge Numeric; the orthogonal distance of the axis label from
#'   the axis, as a proportion of the minimum of the plot width and height.
#' @param tick_length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @param text_dodge Numeric; the orthogonal distance of tick mark text from the
#'   axis, as a proportion of the minimum of the plot width and height.
#' @param axis.colour,axis.color,axis.alpha Default aesthetics for axes. Set to
#'   NULL to inherit from the data's aesthetics.
#' @param label.angle,label.colour,label.color,label.alpha Default aesthetics
#'   for labels. Set to NULL to inherit from the data's aesthetics.
#' @param tick.linewidth,tick.colour,tick.color,tick.alpha Default aesthetics
#'   for tick marks. Set to NULL to inherit from the data's aesthetics.
#' @param text.size,text.angle,text.hjust,text.vjust,text.family,text.fontface,text.colour,text.color,text.alpha
#'   Default aesthetics for tick mark labels. Set to NULL to inherit from the
#'   data's aesthetics.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-axis.r
#' @export
geom_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
  by = NULL, num = NULL,
  tick_length = .025, text_dodge = .03, label_dodge = .03,
  ...,
  axis.colour = NULL, axis.color = NULL, axis.alpha = NULL,
  label.angle = 0,
  label.colour = NULL, label.color = NULL, label.alpha = NULL,
  # TODO: Inherit from theme.
  tick.linewidth = 0.25,
  tick.colour = NULL, tick.color = NULL, tick.alpha = NULL,
  # TODO: Inherit from theme.
  text.size = 2.6,
  text.angle = 0, text.hjust = 0.5, text.vjust = 0.5,
  # TODO: Inherit from theme.
  text.family = NULL, text.fontface = NULL,
  text.colour = NULL, text.color = NULL, text.alpha = NULL,
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
      # NB: This is why Teun switched to `<element>_gp = list(...)`.
      axis.colour = axis.color %||% axis.colour,
      axis.alpha = axis.alpha,
      label.angle = label.angle,
      label.colour = label.color %||% label.colour,
      label.alpha = label.alpha,
      tick.linewidth = tick.linewidth,
      tick.colour = tick.color %||% tick.colour,
      tick.alpha = tick.alpha,
      text.size = text.size,
      text.angle = text.angle,
      text.hjust = text.hjust,
      text.vjust = text.vjust,
      text.family = text.family,
      text.fontface = text.fontface,
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
GeomAxis <- ggproto(
  "GeomAxis", Geom,
  
  required_aes = c("x", "y"),
  non_missing_aes = c("x", "y", "angle", "radius"),
  optional_aes = c(
    "lower", "upper",
    "yintercept", "xintercept", "xend", "yend"
  ),
  
  default_aes = aes(
    # axis & label
    linetype = "solid", linewidth = .25, size = 3.88,
    angle = 0, hjust = 0.5, vjust = 0.5,
    colour = "black", alpha = NA,
    label = "", family = "", fontface = 1L,
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
    
    # limits?
    use_limits <- ! is.null(data[["lower"]]) && ! is.null(data[["upper"]])
    # offset?
    use_offset <- 
      ! is.null(data[["yintercept"]]) || ! is.null(data[["xintercept"]]) ||
      (! is.null(data[["xend"]])       && ! is.null(data[["yend"]]))
    
    # compute endpoints
    if (use_limits) {
      data <- transform(
        data,
        xmin = lower * cos(angle), ymin = lower * sin(angle),
        xmax = upper * cos(angle), ymax = upper * sin(angle)
      )
    }
    
    # recover and offset endpoints
    if (use_offset) {
      if (is.null(data[["xend"]]) || is.null(data[["yend"]]))
        data <- recover_offset_endpoints(data)
      
      if (use_limits) {
        data <- transform(
          data,
          xmin = xmin + xend, ymin = ymin + yend,
          xmax = xmax + xend, ymax = ymax + yend
        )
      }
    }
    
    # drop position coordinates
    data$x <- data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
    by = NULL, num = NULL,
    tick_length = .025, text_dodge = .03, label_dodge = .03,
    axis.colour = NULL, axis.alpha = NULL,
    label.angle = 0,
    label.colour = NULL, label.alpha = NULL,
    tick.linewidth = 0.25,
    tick.colour = NULL, tick.alpha = NULL,
    text.size = 2.6,
    text.angle = 0, text.hjust = 0.5, text.vjust = 0.5,
    text.family = NULL, text.fontface = NULL,
    text.colour = NULL, text.alpha = NULL,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    # save(data, panel_params, coord,
    #      axis_labels, axis_ticks, axis_text,
    #      by, num,
    #      tick_length, text_dodge, label_dodge,
    #      axis.colour, axis.alpha,
    #      label.angle,
    #      label.colour, label.alpha,
    #      tick.linewidth,
    #      tick.colour, tick.alpha,
    #      text.size,
    #      text.angle, text.hjust, text.vjust,
    #      text.family, text.fontface,
    #      text.colour, text.alpha,
    #      parse, check_overlap,
    #      na.rm,
    #      file = "geom-axis-draw-panel.rda")
    # load(file = "geom-axis-draw-panel.rda")
    
    data <- ensure_cartesian_polar(data)
    
    if (! coord$is_linear())
      warning("Axes are not yet tailored to non-linear coordinates.")
    
    # extract value ranges
    ranges <- coord$range(panel_params)
    
    # remove lengthless vectors
    data <- subset(data, x^2 + y^2 > 0)
    
    # offset?
    use_offset <- ! is.null(data[["xend"]]) && ! is.null(data[["yend"]])
    
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # recover slope and (if offset) intercepts
    if (is.null(data[["slope"]])) data$slope <- data$y / data$x
    if (use_offset) {
      if (is.null(data[["yintercept"]]) || is.null(data[["xintercept"]]))
        data <- recover_offset_intercepts(data)
    }
    
    # text dodge vector
    if (axis_labels || axis_text) {
      data <- transform(
        data,
        dodge_angle = if (use_offset) 
          atan2(yend, xend) 
        else 
          (atan(slope) + pi/2)
      )
    }
    
    # compute marks (`x_t` and `y_t`):
    # if no segments then first bound outside window
    if (axis_ticks || axis_text) {
      mark_data <- data
      
      # compute rule bounds in axis units, just beyond window borders
      mark_data <- delimit_rules(mark_data, ranges$x, ranges$y)
      
      # calculate rule values and positions
      mark_data <- calibrate_rules(mark_data, by, num, loose = FALSE)
    }
    
    # axis grobs: if `xend` & `yend` then segment else abline & vline
    axis_data <- unique(data)
    # specify independent aesthetics
    axis_data$colour <- axis.colour %||% axis_data$colour
    axis_data$alpha <- axis.alpha %||% axis_data$alpha
    
    # NB: This step redefines positional aesthetics for a specific grob.
    
    # diagonal versus vertical lines
    axis_data <- transform(
      axis_data,
      vline = (x == 0 & y != 0) | (angle %% pi == pi/2),
      slope = tan(angle)
    )
    axis_data <- transform(
      axis_data,
      intercept = ifelse(vline, Inf, axis_data$yintercept %||% 0),
      xintercept = ifelse(slope == 0, Inf, axis_data$xintercept %||% 0)
    )
    
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
    
    if (axis_labels) {
      label_data <- data
      # specify independent aesthetics
      label_data$colour <- label.colour %||% label_data$colour
      label_data$alpha <- label.alpha %||% label_data$alpha
      
      # NB: This step redefines positional aesthetics for a specific grob.
      
      # compute positions: if `xend` & `yend` then mid/endpoint else border
      if (use_offset) {
        label_data <- border_points_offset(
          label_data,
          panel_params$x.range, panel_params$y.range
        )
      } else {
        label_data <- border_points_origin(
          label_data,
          panel_params$x.range, panel_params$y.range
        )
      }
      # adjust labels inward from borders
      label_data <- transform(
        label_data,
        hjust = ifelse(x < 0, 0, 1)
      )
      
      # dodge axis
      label_data <- transform(
        label_data,
        x = x + cos(dodge_angle) * plot_whmin * label_dodge,
        y = y + sin(dodge_angle) * plot_whmin * label_dodge
      )
      # update text angle
      label_data <- transform(
        label_data,
        angle = atan(tan(angle)) + label.angle
      )
      # put total angle in degrees
      label_data$angle <- label_data$angle * 180 / pi
      
      # axis label grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = label_data,
        panel_params = panel_params, coord = coord
      )))
      
    }
    
    if (axis_ticks) {
      tick_data <- mark_data
      # specify independent aesthetics
      tick_data$linewidth <- tick.linewidth %||% tick_data$linewidth
      tick_data$colour <- tick.colour %||% tick_data$colour
      tick_data$alpha <- tick.alpha %||% tick_data$alpha
      
      # tick mark radius
      rtick <- plot_whmin * tick_length / 2
      # tick mark vector
      tick_data <- transform(
        tick_data,
        xtick = - y / radius * rtick,
        ytick =   x / radius * rtick
      )
      
      # NB: This step redefines positional aesthetics for a specific grob.
      
      # endpoints of tick marks
      tick_data <- transform(
        tick_data,
        xend = x_t - xtick, x = x_t + xtick,
        yend = y_t - ytick, y = y_t + ytick
      )
      
      # tick mark grobs
      grobs <- c(grobs, list(GeomSegment$draw_panel(
        data = offset_xy(tick_data),
        panel_params = panel_params, coord = coord
      )))
      
    }
    
    if (axis_text) {
      text_data <- mark_data
      # specify independent aesthetics
      text_data$size <- text.size %||% text_data$size
      # text_data$angle <- text.angle
      text_data$hjust <- text.hjust
      text_data$vjust <- text.vjust
      text_data$family <- text.family %||% text_data$family
      text_data$fontface <- text.fontface %||% text_data$fontface
      text_data$colour <- text.colour %||% text_data$colour
      text_data$alpha <- text.alpha %||% text_data$alpha
      
      # omit labels at origin
      if (! use_offset) {
        text_data <-
          text_data[text_data$x_t != 0 | text_data$y_t != 0, , drop = FALSE]
      }
      
      # NB: This step redefines positional aesthetics for a specific grob.
      
      # dodge axis
      text_data <- transform(
        text_data,
        x = x_t - cos(dodge_angle) * plot_whmin * text_dodge,
        y = y_t - sin(dodge_angle) * plot_whmin * text_dodge
      )
      # update text angle and put in degrees
      text_data <- transform(
        text_data,
        angle = (atan(tan(angle)) + text.angle) * 180 / pi
      )
      
      if (nrow(text_data) > 0L) {
        # mark text grobs
        grobs <- c(grobs, list(GeomText$draw_panel(
          data = offset_xy(text_data),
          panel_params = panel_params, coord = coord,
          parse = parse,
          check_overlap = check_overlap,
          na.rm = na.rm
        )))
      }
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_axis")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)
