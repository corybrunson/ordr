#' @title Render axes through origin
#' 

#' @description `geom_*_axis()` renders lines through the origin and the
#'   position of each case or variable.
#' @template biplot-layers

#' @section Aesthetics:

#' `geom_*_axis()` understands the following aesthetics (required aesthetics are
#' in bold):

#' - **`x`**
#' - **`y`**
#' - `alpha`
#' - `colour`
#' - `linetype`
#' - `size`
#' - `group`
#' 

#' @name geom-biplot-axis
#' @import ggplot2
#' @inheritParams ggplot2::layer
#' @template param-geom
#' @template param-matrix
#' @example inst/examples/diabetes-lda-axes.r
NULL

#' @rdname geom-biplot-axis
#' @usage NULL
#' @export
GeomAxis <- ggproto(
  "GeomAxis", GeomAbline,
  
  required_aes = c("x", "y"),
  
  setup_data = function(data, params) {
    
    # diagonal versus vertical lines
    data$vline <- data$x == 0 & data$y != 0
    # diagonal line columns
    data$intercept <- rep(0, nrow(data))
    data$slope <- data$y / data$x
    # vertical line columns
    data$xintercept <- rep(0, nrow(data))
    # remove position columns
    # (prevent coordinates from affecting position limits)
    data$x <- NULL
    data$y <- NULL
    
    data
  },
  
  draw_panel = function(
    data, panel_params, coord,
    na.rm = FALSE
  ) {
    if (! coord$is_linear()) {
      warning("Axes are not yet tailored to non-linear coordinates.")
    }
    
    # combine line grobs
    grobs <- list()
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
    grob <- do.call(grid::grobTree, grobs)
    grob$name <- grid::grobName(grob, "geom_axis")
    grob
  }
)

#' @rdname geom-biplot-axis
#' @export
geom_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
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
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-axis
#' @export
geom_u_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = u_stat(stat),
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-axis
#' @export
geom_v_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = v_stat(stat),
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}

#' @rdname geom-biplot-axis
#' @export
geom_biplot_axis <- function(
  mapping = NULL, data = NULL, stat = "identity", position = "identity",
  .matrix = "v",
  ...,
  na.rm = FALSE,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = matrix_stat(.matrix, stat),
    geom = GeomAxis,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = na.rm,
      ...
    )
  )
}
