#' @title Pins emanating from the abscissa
#'
#' @description `geom_pin()` renders segments orthogonal to the abscissa,
#'   optionally with outward-facing labels.

#' @template biplot-layers

#' @section Aesthetics:

#' `geom_pin()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `linetype`
#' - `linewidth`
#' - `colour`
#' - `alpha`
#' - `label`
#' - `size`, `angle`,
#'   `family`, `fontface`,
#'   `label_colour`, `label_alpha`
#' - `group`
#' 

#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @inheritParams ggplot2::geom_linerange
#' @inheritParams ggplot2::geom_segment
#' @inheritParams ggplot2::geom_text
#' @param length A [grid::unit()] object that sets the length of the pins. Use
#'   scale expansion to avoid overplotting of data.
#' @param pin_labels Logical; whether to append labels to the pins.
#' @template param-geom
#' @template return-layer
#' @family geom layers
#' @example inst/examples/ex-geom-pin.r
#' @export
geom_pin <- function(
    mapping = NULL, data = NULL, stat = "identity", position = "identity",
    length = unit(0.2, "npc"), lineend = "round",
    pin_labels = TRUE,
    orientation = NA,
    ...,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE,
    show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomPin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      length = length,
      lineend = lineend,
      pin_labels = pin_labels,
      orientation = orientation,
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
GeomPin <- ggproto(
  "GeomPin", GeomLinerange,
  
  # `ymin|xmin` will be set to 0
  required_aes = c("x|y", "ymax|xmax"),
  
  default_aes = aes(
    # pin
    linetype = 1, linewidth = 0.5, colour = "black", alpha = NA,
    # pin label
    label = "",
    size = 3.88, angle = 0,
    family = "", fontface = 1,
    label_colour = "black", label_alpha = NA
  ),
  
  setup_params = function(data, params) {
    params$flipped_aes <- 
      has_flipped_aes(data, params, range_is_orthogonal = TRUE)
    if (! (params$flipped_aes || 
           all(c("x", "ymax") %in% c(names(data), names(params))))) {
      stop("Either, `x` and `ymax` or `y` and `xmax` must be supplied.")
    }
    params
  },
  
  setup_data = function(data, params) {
    
    # prepare position aesthetics for `GeomSegment`, flipping if necessary
    data <- flip_data(data, params$flipped_aes)
    data <- with(params, transform(data, xend = x, y = 0, yend = ymax))
    data <- flip_data(data, params$flipped_aes)
    
    data
  },
  
  extra_params = c("na.rm", "orientation"),
  
  draw_panel = function(
    data, panel_params, coord,
    length = unit(0.2, "npc"), lineend = "round",
    pin_labels = TRUE,
    flipped_aes = FALSE,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    
    # initialize grob list
    grobs <- list()
    
    # call `GeomSegment`
    grobs <- c(grobs, list(GeomSegment$draw_panel(
      data, panel_params, coord,
      lineend = lineend, na.rm = na.rm
    )))
    
    if (pin_labels) {
      label_data <- data
      
      # specify independent aesthetics
      label_data$colour <- label_data$label_colour
      label_data$alpha <- label_data$label_alpha
      
      # specify positions
      data <- flip_data(data, flipped_aes)
      label_data <- transform(label_data, y = ymax, vjust = "outward")
      data <- flip_data(data, flipped_aes)
      
      # rotate labels from orthogonal orientation
      label_data <- transform(label_data, angle = angle + 90 * flipped_aes)
      
      # pin label grobs
      grobs <- c(grobs, list(GeomText$draw_panel(
        data = label_data,
        panel_params = panel_params, coord = coord,
        parse = parse,
        check_overlap = check_overlap,
        na.rm = na.rm
      )))
      
    }
    
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_pin")
    grob
  }
)
