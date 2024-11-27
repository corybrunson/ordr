#' @title Rulers offset from the origin
#'
#' @description `geom_rule()` renders segments offset from axes through the
#'   origin and the position of each case or variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_rule()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`x`**
#' - **`y`**
#' - `x0`
#' - `y0`
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
#' The position aesthetics `x0` and `y0` encode the offset vector.
#' 

#' @import ggplot2
#' @inheritParams geom_axis
#' @template param-geom
#' @param rule_labels,rule_ticks,rule_text Logical; whether to include labels,
#'   tick marks, and text value marks along the rules.
#' @param tick_length Numeric; the length of the tick marks, as a proportion of
#'   the minimum of the plot width and height.
#' @param text_dodge Numeric; the orthogonal distance of the text from the rule,
#'   as a proportion of the minimum of the plot width and height.
#' @template return-layer
#' @family geom layers
#' @examples
#' @export
geom_rule <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  rule_labels = TRUE, rule_ticks = TRUE, rule_text = TRUE,
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
    geom = GeomRule,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      rule_labels = rule_labels, rule_ticks = rule_ticks, rule_text = rule_text,
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
GeomRule <- ggproto(
  "GeomRule", GeomAxis,
  
  required_aes = c("x", "y", "lower", "upper"),
  
  draw_panel = function(
    data, panel_params, coord,
    rule_labels = TRUE, rule_ticks = TRUE, rule_text = TRUE,
    by = NULL, num = NULL,
    tick_length = .025, text_dodge = .03, label_dodge = .03,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    if (! coord$is_linear()) {
      warning("Rules are not yet tailored to non-linear coordinates.")
    }
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
    # extract value ranges
    ranges <- coord$range(panel_params)
    
    # prepare for marks (calculate `x_val` and `y_val`)
    mark_data <- calibrate_rules(data, by, num)
    # truncate axes to `GeomSegment` position aesthetics
    # FIXME: Use `nest()` in `calibrate_rules()` to prevent ambiguity.
    bound_data <- mark_data |> 
      dplyr::group_by(dplyr::across(
        -tidyselect::all_of(c("label", "x_val", "y_val"))
      )) |> 
      dplyr::summarize(
        x = dplyr::first(x_val), xend = dplyr::last(x_val),
        y = dplyr::first(y_val), yend = dplyr::last(y_val)
      ) |> 
      dplyr::ungroup() |> 
      as.data.frame()
    
    # initialize grob list
    grobs <- list()
    
    # minimum of the plot width and height
    plot_whmin <- min(diff(ranges$x), diff(ranges$y))
    
    # rule grobs: combination of line grobs
    # FIXME: Why don't rules force plotting window to expand?
    grobs <- c(grobs, list(GeomSegment$draw_panel(
      data = offset_xy(unique(bound_data)),
      panel_params = panel_params, coord = coord
    )))
    
    if (rule_ticks) {
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
    
    if (rule_text) {
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
    
    if (rule_labels) {
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
      
      # FIXME: Position labels at rule end nearest origin.
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
      
      # dodge rule
      label_data <- transform(
        label_data,
        x = x + axis_y / sqrt(axis_ss) * plot_whmin * label_dodge,
        y = y - axis_x / sqrt(axis_ss) * plot_whmin * label_dodge
      )
      
      # rule label grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = offset_xy(label_data),
        panel_params = panel_params, coord = coord
      )))
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_rule")
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
  angle <- atan2(data$axis_y, data$axis_x)
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
