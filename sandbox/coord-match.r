#' Cartesian coordinates with fixed aspect ratio and consistent annotation
#' 

#' \code{coord_match} is a shortcut for \code{ggplot2::coord_fixed(ratio = 1)}
#' that also forces the major and minor breaks to follow the same regular
#' positioning scheme.

#' @name ggbiplot-coord
#' @import ggplot2
#' @inheritParams ggplot2::coord_cartesian

#' @rdname ggbiplot-coord
#' @export
CoordMatch <- ggproto(
  "CoordMatch", CoordFixed,
  
  aspect = function(ranges) {
    diff(ranges$y.range) / diff(ranges$x.range)
  },
  
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    save(self, scale_x, scale_y, params, file = "temp-coord.rda")
    load("temp-coord.rda")
    
    range_xy <- range(c(
      scale_x$range$range,
      scale_y$range$range
    ))
    
    train_match <- function(scale, limits, name) {
      range <- scale_range_match(scale, range = range_xy, limits, self$expand)
      
      out <- scale$break_info(range_xy)
      out$arrange <- scale$axis_order()
      names(out) <- paste(name, names(out), sep = ".")
      out
    }
    
    out <- c(
      train_match(scale_x, self$limits$x, "x"),
      train_match(scale_y, self$limits$y, "y")
    )
    
    within_range <- function(x, ran) {
      x > ran[1] & x < ran[2]
    }
    
    out$x.range <- scale_x$range$range
    wh_major <- which(within_range(out$x.major_source, out$x.range))
    out$x.labels <- out$x.labels[wh_major]
    out$x.major <- out$x.major[wh_major]
    out$x.major_source <- out$x.major_source[wh_major]
    wh_minor <- which(within_range(out$x.minor_source, out$x.range))
    out$x.minor <- out$x.minor[wh_minor]
    out$x.minor_source <- out$x.minor_source[wh_minor]
    
    out$y.range <- scale_y$range$range
    wh_major <- which(within_range(out$y.major_source, out$y.range))
    out$y.labels <- out$y.labels[wh_major]
    out$y.major <- out$y.major[wh_major]
    out$y.major_source <- out$y.major_source[wh_major]
    wh_minor <- which(within_range(out$y.minor_source, out$y.range))
    out$y.minor <- out$y.minor[wh_minor]
    out$y.minor_source <- out$y.minor_source[wh_minor]
    
    out
  }
)

#' @rdname ggbiplot-coord
#' @export
coord_match <- function(xlim = NULL, ylim = NULL, expand = TRUE, clip = "on") {
  ggproto(
    NULL, CoordMatch,
    limits = list(x = xlim, y = ylim),
    expand = expand,
    clip = clip
  )
}

scale_range_match <- function(scale, range, limits = NULL, expand = TRUE) {
  expansion <- if (expand) expand_default(scale) else c(0, 0)
  
  if (is.null(limits)) {
    scale$dimension(expansion)
  } else {
    range <- range(scale$transform(limits))
    scales::expand_range(range, expansion[1], expansion[2])
  }
}

expand_default <- function(
  scale, discrete = c(0, 0.6, 0, 0.6), continuous = c(0.05, 0, 0.05, 0)
) {
  waive_expand <- inherits(scale$expand, "waiver")
  if (! waive_expand) {
    scale$expand
  } else if (scale$is_discrete()) {
    discrete
  } else {
    continuous
  }
}
