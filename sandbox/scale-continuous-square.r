#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
ScaleContinuousSquare <- ggproto(
  "ScaleContinuousSquare", ScaleContinuousPosition,
  
  dimension = function(self, expand = c(0, 0, 0, 0)) {
    print(self$get_limits())
    ggplot2:::expand_range4(self$get_limits(), expand)
  }
  
)

#' Squared position scales for continuous data (x & y)
#' 
#' @name scale_square

#' @rdname scale_square
#'
#' @param sec.axis [sec_axis()] is used to specify a secondary axis.
#'
#' @export
scale_x_square <- function(
    name = waiver(), breaks = waiver(),
    minor_breaks = waiver(), n.breaks = NULL,
    labels = waiver(), limits = NULL,
    expand = waiver(), oob = scales:::censor,
    na.value = NA_real_, trans = "identity",
    guide = waiver(), position = "bottom",
    sec.axis = waiver()
) {
  sc <- continuous_scale(
    ggplot2:::ggplot_global$x_aes,
    "position_c", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousSquare
  )
  
  ggplot2:::set_sec_axis(sec.axis, sc)
  
}

#' @rdname scale_square
#'
#' @param sec.axis [sec_axis()] is used to specify a secondary axis.
#'
#' @export
scale_y_square <- function(
    name = waiver(), breaks = waiver(),
    minor_breaks = waiver(), n.breaks = NULL,
    labels = waiver(), limits = NULL,
    expand = waiver(), oob = scales:::censor,
    na.value = NA_real_, trans = "identity",
    guide = waiver(), position = "left",
    sec.axis = waiver()
) {
  sc <- continuous_scale(
    ggplot2:::ggplot_global$y_aes,
    "position_c", identity, name = name, breaks = breaks, n.breaks = n.breaks,
    minor_breaks = minor_breaks, labels = labels, limits = limits,
    expand = expand, oob = oob, na.value = na.value, trans = trans,
    guide = guide, position = position, super = ScaleContinuousSquare
  )
  
  ggplot2:::set_sec_axis(sec.axis, sc)
  
}
