#' @title Render isolines (contour lines) along axes
#'
#' @description `geom_isolines()` renders isolines along row or column axes.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_isolines()` understands the following aesthetics (required aesthetics
#' are in bold):

#' - **`x`**
#' - **`y`**
#' - `colour`
#' - `alpha`
#' - `size`
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
#' @family geom layers
#' @example inst/examples/ex-geom-isolines-diabetes.r
#' @export
geom_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  label_dodge = .1,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomIsolines,
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

#' @rdname biplot-geoms
#' @export
geom_rows_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  label_dodge = .1,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = rows_stat(stat),
    geom = GeomIsolines,
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

#' @rdname biplot-geoms
#' @export
geom_cols_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  label_dodge = .1,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = cols_stat(stat),
    geom = GeomIsolines,
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

#' @rdname biplot-geoms
#' @export
geom_dims_isolines <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "cols",
  isoline_text = TRUE,
  by = NULL, num = NULL,
  label_dodge = .1,
  ...,
  parse = FALSE, check_overlap = FALSE,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomIsolines,
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
GeomIsolines <- ggproto(
  "GeomIsolines", Geom,
  
  required_aes = c("x", "y"),
  
  default_aes = aes(
    # isoline
    colour = "black", alpha = .8,
    size = .5, linetype = "dashed",
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
    label_dodge = .1,
    parse = FALSE, check_overlap = FALSE,
    na.rm = TRUE
  ) {
    
    # prepare for marks
    ranges <- coord$range(panel_params)
    data <- calibrate_axes(data, ranges, by, num)
    
    # initialize grob list
    grobs <- list()
    
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
      # -+- within plotting window -+-
      text_data <- transform(
        text_data,
        x = x_val + axis_x / sqrt(axis_ss) * label_dodge,
        y = y_val + axis_y / sqrt(axis_ss) * label_dodge
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
    grob$name <- grid::grobName(grob, "geom_isolines")
    grob
  },
  
  # update this to include segment and letter in key squares
  draw_key = draw_key_abline
)

# `data` must have fields 'axis_x' and 'axis_y'
calibrate_axes <- function(data, ranges, by, num) {
  
  if (is.null(by) && is.null(num)) num <- 6L
  
  # window boundaries for axis positions
  data$win_xmin <- ifelse(data$axis_x > 0, ranges$x[[1L]], ranges$x[[2L]])
  data$win_xmax <- ifelse(data$axis_x > 0, ranges$x[[2L]], ranges$x[[1L]])
  data$win_ymin <- ifelse(data$axis_y > 0, ranges$y[[1L]], ranges$y[[2L]])
  data$win_ymax <- ifelse(data$axis_y > 0, ranges$y[[2L]], ranges$y[[1L]])
  # vector lengths
  data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
  # project window corners onto axis (isoline extrema), in axis units
  data$axis_min <-
    (data$win_xmin * data$axis_x + data$win_ymin * data$axis_y) / data$axis_ss
  data$axis_max <-
    (data$win_xmax * data$axis_x + data$win_ymax * data$axis_y) / data$axis_ss
  data$win_xmin <- NULL
  data$win_xmax <- NULL
  data$win_ymin <- NULL
  data$win_ymax <- NULL
  
  # label ranges
  data$label_min <- data$center + data$scale * data$axis_min
  data$label_max <- data$center + data$scale * data$axis_max
  data$axis_min <- NULL
  data$axis_max <- NULL
  
  # element units; by default, use Wilkinson's breaks algorithm
  label_vals <- if (is.null(by)) {
    lapply(seq(nrow(data)), function(i) {
      labeling::extended(data$label_min[[i]], data$label_max[[i]], num)
    })
  } else {
    if (length(by) == 1L) by <- rep(by, nrow(data))
    lapply(seq(nrow(data)), function(i) {
      by[[i]] * seq(
        floor(data$label_min[[i]] / by[[i]]),
        ceiling(data$label_max[[i]] / by[[i]])
      )
    })
  }
  data <- data[rep(seq(nrow(data)), sapply(label_vals, length)),
               , drop = FALSE]
  data$label <- unlist(label_vals)
  data$label_min <- NULL
  data$label_max <- NULL
  
  # axis positions
  data$axis_val <- (data$label - data$center) / data$scale
  data$x_val <- data$axis_val * data$axis_x
  data$y_val <- data$axis_val * data$axis_y
  data$axis_val <- NULL
  
  data
}
