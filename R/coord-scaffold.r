#' @title Convenience coordinate system for scaffolding axes
#'
#' @description 2- (and 3-) dimensional biplots require that coordinates lie on
#'   the same scale but may additionally benefit from a square plotting window.
#'   While `CoordRect` provides control of coordinate and window aspect ratios,
#'   the convenience `CoordScaffold` system also fixes the coordinate aspect
#'   ratio at `1` and gives the user control only of the plotting window.
#'
#' @importFrom gggda CoordRect
#' @inheritParams ggplot2::coord_fixed
#' @param window_ratio aspect ratio of plotting window
#' @example inst/examples/ex-coord-scaffold.r
#' @export
coord_scaffold <- function(
    window_ratio = 1,
    xlim = NULL, ylim = NULL, expand = TRUE, clip = "on"
) {
  check_coord_limits(xlim)
  check_coord_limits(ylim)
  ggproto(
    NULL, CoordScaffold,
    limits = list(x = xlim, y = ylim),
    window_ratio = window_ratio,
    expand = expand,
    clip = clip
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
CoordScaffold <- ggproto(
  "CoordScaffold", CoordRect,
  
  # require coordinate aspect ratio to be 1
  aspect = function(self, ranges) {
    diff(ranges$y.range) / diff(ranges$x.range)
  }
)

check_coord_limits <- getFromNamespace("check_coord_limits", "ggplot2")
