#' @title Scaffolding theme
#'
#' @description Omit cartesian coordinate visual aids.
#'
#' @details Geometric data analysis concerns the intrinsic geometry of data.
#'   Analyses often use artificial or arbitrary coordinate systems that carry no
#'   useful interpretation but instead serve as scaffolding, especially for
#'   graphical elements like [axes][geom_axis] that represent other variables
#'   (Gardner, 2001). In such cases, the visual aids (tick marks and labels,
#'   grid lines) used to recover the coordinates of the row and column markers
#'   would add unnecessary clutter and should be omitted. This partial theme
#'   updates the current theme by removing these elements. The biplot theme is
#'   an alias included for convenience and backward compatibility.
#'

#' @template ref-gardner2001

#' @return A ggplot [theme][ggplot2::theme].
#' @export
theme_scaffold <- function() {
  theme(
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  )
}

#' @rdname theme_scaffold
#' @export
theme_biplot <- theme_scaffold
