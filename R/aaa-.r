#' @title ggproto classes created and adapted for ordr
#'
#' @description In addition to geometric element layers (geoms) based on
#'   base-**ggplot2** layers like `geom_point()` but specified to matrix factors
#'   as `geom_row_point()`, **ordr** introduces [ggproto][ggplot2::ggproto]
#'   classes for some additional geometric elements commonly used in biplots.
#'   The factor-specific geoms invoke the statistical transformation layers
#'   (stats) `stat_rows()` and `stat_cols()`, which specify the matrix factor.
#'   Because each ggplot layer consists of only one stat and one geom, this
#'   necessitates that ggproto classes for new stats must also come in `*Rows`
#'   and `*Cols` flavors.
#'
#' @seealso [`ggplot2::ggplot2-ggproto`] and [ggplot2::ggproto] for explanations
#'   of base ggproto classes in **ggplot2** and how to create new ones.
#' @name ordr-ggproto
NULL   
