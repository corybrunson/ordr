#' @title Render tick mark text for axes
#'
#' @description `geom_axis_text()` renders tick mark text for specified axes
#'   among the row or column factors.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_axis_text()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `angle`
#' - `colour`
#' - `family`
#' - `fontface`
#' - `hjust`
#' - `lineheight`
#' - `size`
#' - `vjust`
#' - `group`
#' - `center` (for un-scaling)
#' - `scale` (for un-scaling)
#' 

#' @import ggplot2
#' @include geom-isolines.r
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_isolines
#' @param label_dodge Numeric; the orthogonal distance of the text from
#'   the axis, as a proportion of the minimum of the plot width and height.
#' @family geom layers
#' @example inst/examples/ex-geom-axis-diabetes.r
#' @export
geom_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, label_dodge = .15,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_rows_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, label_dodge = .15,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_cols_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  subset = NULL, by = NULL, num = NULL, label_dodge = .15,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
      by = by, num = num,
      label_dodge = label_dodge,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname biplot-geoms
#' @export
geom_dims_axis_text <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  subset = NULL, by = NULL, num = NULL, label_dodge = .15,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxisText,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      subset = subset,
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
GeomAxisText <- ggproto(
  "GeomAxisText", GeomText,
  
  required_aes = c("x", "y"),
  default_aes = aes(
    colour = "black", alpha = NA, size = 2.6, angle = 0,
    hjust = 0.5, vjust = 0.5, family = "", fontface = 1, lineheight = 1.2
  ),
  
  setup_params = GeomIsolines$setup_params,
  setup_data = GeomIsolines$setup_data,
  
  draw_panel = function(
    data, panel_params, coord,
    subset = NULL, by = NULL, num = NULL, label_dodge = .15,
    parse = FALSE, check_overlap = FALSE,
    na.rm = FALSE
  ) {
    if (is.null(by) && is.null(num)) num <- 6L
    
    ranges <- coord$range(panel_params)
    
    data <- calibrate_axes(data, ranges, by, num)
    
    # text strings
    data <- transform(data, label = format(label, digits = 3))
    
    # omit labels at origin
    data <- data[data$x != 0 | data$y != 0, , drop = FALSE]
    # calculate angles
    if (is.null(data$angle)) data$angle <- 0
    data$angle <-
      as.numeric(data$angle) + atan(- data$axis_x / data$axis_y) / pi * 180
    # dodge axis
    # -+- within plotting window -+-
    data <- transform(
      data,
      x = x_val + axis_x / sqrt(axis_ss) * label_dodge,
      y = y_val + axis_y / sqrt(axis_ss) * label_dodge
    )
    # discard unneeded columns
    data$x_val <- NULL
    data$y_val <- NULL
    
    grob <- GeomText$draw_panel(
      data = data, panel_params = panel_params, coord = coord,
      parse = parse,
      check_overlap = check_overlap,
      na.rm = na.rm
    )
    grob$name <- grid::grobName(grob, "geom_axis_text")
    grob
  }
)
