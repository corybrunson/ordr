#' @title Cartesian coordinates and plotting window with fixed aspect ratios
#'
#' @description Geometric data analysis often requires that coordinates lie on
#'   the same scale. The coordinate system `CoordRect`, alias `CoordSquare`,
#'   provides control of both coordinate and window aspect ratios.
#'
#' @inheritParams ggplot2::coord_fixed
#' @param window_ratio aspect ratio of plotting window
#' @example inst/examples/ex-coord-rect.r
#' @export
coord_rect <- function(
    ratio = 1, window_ratio = ratio,
    xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"
) {
  ggplot2:::check_coord_limits(xlim)
  ggplot2:::check_coord_limits(ylim)
  ggproto(
    NULL, CoordRect,
    limits = list(x = xlim, y = ylim),
    ratio = ratio, window_ratio = window_ratio,
    expand = expand,
    clip = clip
  )
}

#' @rdname coord_rect
#' @usage NULL
#' @export
coord_square <- coord_rect

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordRect <- ggproto(
  "CoordRect", CoordFixed,
  
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    
    # adapted from `CoordCartesian$setup_panel_params`
    
    expansion_x <- ggplot2:::default_expansion(scale_x, expand = self$expand)
    # expansion_x <- ggplot2:::default_expansion(scale_x, expand = params$expand[c(4, 2)])
    limits_x <- scale_x$get_limits()
    continuous_range_x <- ggplot2:::expand_limits_scale(
      scale_x, expansion_x, limits_x, coord_limits = self$limits$x
    )
    aesthetic_x <- scale_x$aesthetics[1]
    
    expansion_y <- ggplot2:::default_expansion(scale_y, expand = self$expand)
    # expansion_y <- ggplot2:::default_expansion(scale_y, expand = params$expand[c(3, 1)])
    limits_y <- scale_y$get_limits()
    continuous_range_y <- ggplot2:::expand_limits_scale(
      scale_y, expansion_y, limits_y, coord_limits = self$limits$y
    )
    aesthetic_y <- scale_y$aesthetics[1]
    
    # synchronize limits and ranges according to `window_ratio` after adjusting
    # for `ratio` (if it is provided; it isn't in `CoordScaffold`)
    adj_ratio <- self$window_ratio / (self$ratio %||% 1)
    limits <- reconcile_rectangle(limits_x, limits_y, adj_ratio)
    continuous_range <- reconcile_rectangle(
      continuous_range_x, continuous_range_y, adj_ratio
    )
    
    view_scales_x <- list(
      ggplot2:::view_scale_primary(scale_x, limits$x, continuous_range$x),
      sec = ggplot2:::view_scale_secondary(scale_x, limits$x, continuous_range$x),
      range = continuous_range$x
    )
    names(view_scales_x) <- 
      c(aesthetic_x, paste0(aesthetic_x, ".", names(view_scales_x)[-1]))
    
    view_scales_y <- list(
      ggplot2:::view_scale_primary(scale_y, limits$y, continuous_range$y),
      sec = ggplot2:::view_scale_secondary(scale_y, limits$y, continuous_range$y),
      range = continuous_range$y
    )
    names(view_scales_y) <- 
      c(aesthetic_y, paste0(aesthetic_y, ".", names(view_scales_y)[-1]))
    
    c(view_scales_x, view_scales_y)
  }
)

reconcile_rectangle <- function(xlim, ylim, ratio) {
  sides <- c(diff(xlim), diff(ylim))
  # by how much to scale each dimension to achieve desired aspect ratio
  sfs <- c(1, ratio) / sides
  sfs <- sfs / min(sfs)
  # new limits
  list(
    x = mean(xlim) + c(-1, 1) * sides[[1]] / 2 * sfs[[1]],
    y = mean(ylim) + c(-1, 1) * sides[[2]] / 2 * sfs[[2]]
  )
}
