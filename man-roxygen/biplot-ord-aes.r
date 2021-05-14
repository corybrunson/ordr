#' @section Ordination aesthetics:

#' The convenience function `[ord_aes()]` can be used to incorporate all
#' coordinates of the ordination model into a statistical transformation. It
#' maps the coordinates to the custom aesthetics `..coord1`, `..coord2`, etc.
#'
#' Some transformations, e.g. `[stat_center()]`, are commutative with projection
#' to the 'x' and 'y' coordinates. If they detect aesthetics of the form
#' `..coord[0-9]+`, then `..coord1` and `..coord2` are converted to `x` and `y`
#' while any remaining are ignored.
#'
#' Other transformations, e.g. `stat_spantree()]`, yield different results in a
#' planar biplot when they are computer before or after projection. If such a
#' stat layer detects these aesthetics, then the lot of them are used in the
#' transformation.
#'
#' In either case, the stat layer returns a data frame with position aesthetics
#' `x` and `y`.
#' 
