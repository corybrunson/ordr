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
    
    # synchronize limits and ranges according to `aspect_ratio` after adjusting
    # for `ratio` (if it is provided; it isn't in `CoordBiplot`)
    adj_ratio <- self$aspect_ratio / (self$ratio %||% 1)
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

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordBiplot <- ggproto(
  "CoordBiplot", CoordRect,
  
  # require coordinate aspect ratio to be 1
  aspect = function(self, ranges) {
    diff(ranges$y.range) / diff(ranges$x.range)
  }
)

#' Cartesian coordinates and plotting window with fixed aspect ratios
#'
#' 2- (and 3-) dimensional biplots require that coordinates lie on the same
#' scale but may additionally benefit from a square plotting window. The
#' general-purpose coordinate system `CoordRect`, alias `CoordSquare`, provides
#' control of both coordinate and window aspect ratios, while the convenience
#' `CoordBiplot` system fixes the coordinate aspect ratio at `1` and gives the
#' user control only of the plotting window.
#'
#' @inheritParams ggplot2::coord_fixed
#' @param aspect_ratio aspect ratio of plotting window
#' @examples
#' # ensures that the resolutions of the axes and the dimensions of the plotting
#' # window respect the specified aspect ratios
#'
#' p <- ggplot(mtcars, aes(mpg, hp/10)) + geom_point()
#' p + coord_rect(ratio = 1)
#' p + coord_rect(ratio = 1, aspect_ratio = 2)
#' p + coord_rect(ratio = 1, aspect_ratio = 1/2)
#' p + coord_rect(ratio = 5)
#' p + coord_rect(ratio = 1/5)
#' p + coord_rect(xlim = c(15, 30))
#' p + coord_rect(ylim = c(15, 30))
#'
#' # Resize the plot to see that the specified aspect ratio is maintained
#' @export
coord_rect <- function(
    ratio = 1, aspect_ratio = ratio,
    xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"
) {
  ggplot2:::check_coord_limits(xlim)
  ggplot2:::check_coord_limits(ylim)
  ggproto(
    NULL, CoordRect,
    limits = list(x = xlim, y = ylim),
    ratio = ratio, aspect_ratio = aspect_ratio,
    expand = expand,
    clip = clip
  )
}

#' @rdname coord_rect
#' @usage NULL
#' @export
coord_square <- coord_rect

#' @rdname coord_rect
#' @examples
#' p <- ggplot(mtcars, aes(mpg, hp/10)) + geom_point()
#' p + coord_biplot()
#' p + coord_biplot(aspect_ratio = 2)
#' 
#' # prevent rescaling in response to `theme()` aspect ratio
#' p <- ggplot(mtcars, aes(mpg, hp/5)) + geom_point()
#' p + coord_equal() + theme(aspect.ratio = 1)
#' p + coord_biplot() + theme(aspect.ratio = 1)
#' 
#' # NB: `theme(aspect.ratio = )` overrides `Coord*$aspect`:
#' p + coord_fixed(ratio = 1) + theme(aspect.ratio = 1)
#' p + coord_biplot(aspect_ratio = 2) + theme(aspect.ratio = 1)
#' @export
coord_biplot <- function(
    aspect_ratio = 1,
    xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"
) {
  ggplot2:::check_coord_limits(xlim)
  ggplot2:::check_coord_limits(ylim)
  ggproto(
    NULL, CoordBiplot,
    limits = list(x = xlim, y = ylim),
    aspect_ratio = aspect_ratio,
    expand = expand,
    clip = clip
  )
}

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
