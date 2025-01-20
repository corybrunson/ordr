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

#' - **`x`**, **`y`**
#' - `lower`, `upper`
#' - `yintercept` _or_ `xintercept` _or_ `xend` and `yend`
#' - `colour`
#' - `alpha`
#' - `linewidth`
#' - `linetype`
#' - `label`
#' - `center`, `scale`
#' - `label_colour`, `label_alpha`, `label_size`, `label_angle`,
#'   `label_family`, `label_fontface`
#' - `tick_colour`, `tick_alpha`, `tick_linewidth`, `tick_linetype`
#' - `text_colour`, `text_alpha`, `text_size`, `text_angle`,
#'   `text_hjust`, `text_vjust`, `text_family`, `text_fontface`
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
  non_missing_aes = c("x", "y", "angle", "radius"),
  optional_aes = c(
    "lower", "upper",
    "yintercept", "xintercept", "xend", "yend"
  ),
  
  default_aes = aes(
    # axis
    colour = "black", alpha = NA,
    linewidth = .25, linetype = "solid",
    # axis label
    label = "",
    label_colour = "black", label_alpha = NA,
    label_size = 3.88, label_angle = 0,
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
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    
    data <- ensure_cartesian_polar(data)
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
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
      label_data$colour <- label_data$label_colour
      label_data$alpha <- label_data$label_alpha
      label_data$size <- label_data$label_size
      label_data$family <- label_data$label_family
      label_data$fontface <- label_data$label_fontface
      
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
        angle = atan(tan(angle)) + label_angle
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
      text_data$colour <- text_data$text_colour
      text_data$alpha <- text_data$text_alpha
      text_data$size <- text_data$text_size
      text_data$hjust <- text_data$text_hjust
      text_data$vjust <- text_data$text_vjust
      text_data$family <- text_data$text_family
      text_data$fontface <- text_data$text_fontface
      
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
        angle = (atan(tan(angle)) + text_angle) * 180 / pi
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
