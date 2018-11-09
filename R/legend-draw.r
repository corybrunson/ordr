#' Biplot key drawing functions
#' 

#' @name ggbiplot-draw-key
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @seealso \code{\link[ggplot2]{draw_key}}
NULL

#' @rdname ggbiplot-draw-key
#' @export
draw_key_line <- function(data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0
  grid::segmentsGrob(
    0, 0.5, 1, 0.5,
    gp = grid::gpar(
      col = alpha(data$colour, data$alpha),
      lwd = data$size * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}

#' @rdname ggbiplot-draw-key
#' @export
draw_key_crosslines <- function(data, params, size) {
  grid::grobTree(
    draw_key_line(data, params, size),
    draw_key_vline(data, params, size)
  )
}

#' @rdname ggbiplot-draw-key
#' @export
draw_key_crosspoint <- function(data, params, size) {
  grid::grobTree(
    draw_key_crosslines(data, params, size),
    draw_key_point(transform(data, size = data$size * 4), params)
  )
}
