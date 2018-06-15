#' @section Biplot layers:

#' \code{\link{ggbiplot}} uses \code{\link[ggplot2]{fortify}} internally to 
#' produce a single data frame with a \code{.matrix} column distinguishing the 
#' subjects (\code{"u"}) and variables (\code{"v"}). The stat layers 
#' \code{stat_u} and \code{stat_v} simply filter the data frame to one of these 
#' two.
#' 
#' The geom layers \code{geom_u_*} and \code{geom_v_*} call the corresponding
#' stat in order to render plot elements for the corresponding matrix \eqn{U} or
#' \eqn{V}. \code{geom_biplot_*} selects a default matrix based on common
#' practice, e.g. \eqn{U} for points and \eqn{V} for arrows.
#' 
