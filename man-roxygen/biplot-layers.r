#' @section Biplot layers:

#' [ggbiplot()] uses [ggplot2::fortify()] internally to produce a single data
#' frame with a `.matrix` column distinguishing the subjects (`"u"`) and
#' variables (`"v"`). The stat layers `stat_u()` and `stat_v()` simply filter
#' the data frame to one of these two.
#'
#' The geom layers `geom_u_*()` and `geom_v_*()` call the corresponding stat in
#' order to render plot elements for the corresponding matrix \eqn{U} or
#' \eqn{V}. `geom_biplot_*()` selects a default matrix based on common practice,
#' e.g. \eqn{U} for points and \eqn{V} for arrows.
#' 
