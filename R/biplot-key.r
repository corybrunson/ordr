#' @title Biplot key drawing functions
#'
#' @description These key drawing functions supplement those built into
#'   **[ggplot2][ggplot2::ggplot2]** for producing legends suitable to biplots.

#' @details
#'
#' `draw_key_line()` is a horizontal counterpart to [ggplot2::draw_key_vline()].
#' `draw_key_crosslines()` superimposes these two keys, and
#' `draw_key_crosspoint()` additionally superimposes an oversized
#' [ggplot2::draw_key_point()].

#' @name draw-key
#' @inheritParams ggplot2::draw_key
#' @return A grid grob.
#' @example inst/examples/ex-ggbiplot-key-iris.r
#' @seealso [ggplot2::draw_key] for key glyphs installed with **ggplot2**.
NULL

#' @rdname draw-key
#' @export
draw_key_line <- function(data, params, size) {
  data$linetype[is.na(data$linetype)] <- 0
  grid::segmentsGrob(
    0, 0.5, 1, 0.5,
    gp = grid::gpar(
      col = alpha(data$colour, data$alpha),
      lwd = (data$linewidth %||% data$size) * .pt,
      lty = data$linetype,
      lineend = "butt"
    )
  )
}

#' @rdname draw-key
#' @export
draw_key_crosslines <- function(data, params, size) {
  grid::grobTree(
    draw_key_line(data, params, size),
    draw_key_vline(data, params, size)
  )
}

#' @rdname draw-key
#' @export
draw_key_crosspoint <- function(data, params, size) {
  grid::grobTree(
    draw_key_crosslines(data, params, size),
    draw_key_point(transform(data, size = data$size * 4), params)
  )
}
