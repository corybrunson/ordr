#' @title Biplot theme
#'
#' @description Omit coordinate visual aids from biplots.
#'
#' @details
#'
#' Because the artificial axes often go uninterpreted, biplots may omit the
#' visual aids (tick marks and labels, grid lines) used to recover the
#' artificial coordinates of the row and column markers The biplot (partial)
#' theme removes these elements from the current theme. This can be especially
#' helpful when plotting [axes][geom_axis()] or [isolines][geom_isoline()].
#' 
#' @return A ggplot [theme][ggplot2::theme].
#' @export
theme_biplot <- function() {
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
}
