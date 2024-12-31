#' @title Rulers through or offset from the origin
#'
#' @description `geom_rule()` renders segments through or orthogonally
#'   translated from the origin.

#' @details As implemented here, a rule is just an [axis][geom_axis] that has a
#'   fixed range, usually the limits of the data. See [stat_rule()] for further
#'   details.
#' 

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_rule()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`x`**, **`y`**
#' - **`lower`, `upper`**
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

#' @template param-tick
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_axis
#' @template param-geom
#' @param snap_rule Logical; whether to snap rule segments to grid values.
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-rule.r
#' @export
geom_rule <- function(
  mapping = NULL, data = NULL, stat = "rule", position = "identity",
  axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
  by = NULL, num = NULL,
  snap_rule = TRUE,
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
      axis_labels = axis_labels, axis_ticks = axis_ticks, axis_text = axis_text,
      by = by, num = num,
      snap_rule = snap_rule,
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
  optional_aes = c("yintercept", "xintercept", "xend", "yend"),
  
  setup_data = function(data, params) {
    
    data <- ensure_cartesian_polar(data)
    
    # offset?
    use_offset <- 
      ! is.null(data[["yintercept"]]) ||
      ! is.null(data[["xintercept"]]) ||
      (! is.null(data[["xend"]]) && ! is.null(data[["yend"]]))
    
    # compute endpoints
    data <- transform(
      data,
      xmin = lower * cos(angle), ymin = lower * sin(angle),
      xmax = upper * cos(angle), ymax = upper * sin(angle)
    )
    
    # recover and offset endpoints
    if (use_offset) {
      if (is.null(data[["xend"]]) || is.null(data[["yend"]]))
        data <- recover_offset_endpoints(data)
      
      data <- transform(
        data,
        xmin = xmin + xend, ymin = ymin + yend,
        xmax = xmax + xend, ymax = ymax + yend
      )
    }
    
    # drop position coordinates
    data$x <- data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    axis_labels = TRUE, axis_ticks = TRUE, axis_text = TRUE,
    by = NULL, num = NULL,
    snap_rule = TRUE,
    tick_length = .025, text_dodge = .03, label_dodge = .03,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    
    # copy `linewidth` to `size` for earlier **ggplot2** versions
    data$size <- data$linewidth
    
    if (! coord$is_linear())
      warning("Axes are not yet tailored to non-linear coordinates.")
    
    # extract value ranges
    ranges <- coord$range(panel_params)
    
    data <- ensure_cartesian_polar(data)
    
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
      
      # calculate rule values and positions
      mark_data <- calibrate_rules(mark_data, by, num, loose = TRUE)
    }
    
    # axis grobs: if `xend` & `yend` then segment else abline & vline
    axis_data <- unique(data)
    
    # NB: This step redefines positional aesthetics for a specific grob.
    
    if (snap_rule) {
      
      # compute extended value range
      mark_data |> 
        dplyr::transmute(axis, label, x = x_t + x_0, y = y_t + y_0) |> 
        dplyr::group_by(axis) |> 
        dplyr::filter(label == min(label) | label == max(label)) |> 
        dplyr::mutate(ext = ifelse(label == min(label), "min", "max")) |> 
        dplyr::filter(all(c("min", "max") %in% ext)) |>
        dplyr::ungroup() |> 
        dplyr::distinct() |>
        tidyr::pivot_wider(
          id_cols = axis,
          names_from = ext, values_from = c(x, y), names_sep = ""
        ) -> 
        mark_range
      
      # extend segment to value range (when available)
      mark_axes <- match(axis_data$axis, mark_range$axis)
      mark_axes <- mark_axes[! is.na(mark_axes)]
      if (length(mark_axes) > 0L) {
        axis_data[mark_axes, c("xend", "yend", "x", "y")] <- 
          mark_range[, c("xmin", "ymin", "xmax", "ymax")]
      }
      if (length(mark_axes) < nrow(axis_data)) {
        axis_data <- subset(axis_data, axis_data$axis %in% mark_axes)
      }
      
    } else {
      
      # recognized segment positions
      axis_data <- transform(
        axis_data,
        xend = xmin, yend = ymin, x = xmax, y = ymax
      )
      
    }
    
    grobs <- c(grobs, list(GeomSegment$draw_panel(
      data = axis_data,
      panel_params = panel_params, coord = coord
    )))
    
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
      # replace x,y with heads then opt for any positions closer to the origin
      # replace x,y with heads or tails, whichever is farther from the origin
      repl_min <- with(label_data, xmin^2 + ymin^2 > xmax^2 + ymax^2)
      label_data <- transform(
        label_data,
        x = ifelse(repl_min, xmin, xmax),
        y = ifelse(repl_min, ymin, ymax)
      )
      # adjust labels inward from borders
      label_data <- transform(
        label_data,
        hjust = ifelse(
          xmin < xmax,
          as.numeric(1 - repl_min),
          as.numeric(repl_min)
        )
      )
      label_data <- subset(label_data, select = -c(xmin, ymin, xmax, ymax))
      if (use_offset) label_data <- subset(label_data, select = -c(xend, yend))
      
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
    grob$name <- grid::grobName(grob, "geom_rule")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)
