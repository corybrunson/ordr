#' @section Biplot layers:

#' [ggbiplot()] uses [ggplot2::fortify()] internally to produce a single data
#' frame with a `.matrix` column distinguishing the subjects (`"rows"`) and
#' variables (`"cols"`). The stat layers `stat_rows()` and `stat_cols()` simply
#' filter the data frame to one of these two.
#'
#' The geom layers `geom_rows_*()` and `geom_cols_*()` call the corresponding
#' stat in order to render plot elements for the corresponding factor matrix.
#' `geom_biplot_*()` selects a default matrix based on common practice, e.g.
#' points for rows and arrows for columns.
#' 
