
#' Site-species occurrence data for finches on the Galapagos islands
#' 
#' These data are taken from Sanderson (2000). They are available in a different
#' format in the \strong{\link[cooccur]{cooccur}} package.
#' 
#' @format A \code{\link{data.frame}}.
#' @keywords datasets
#' @source Sanderson J.G. (2000). Testing Ecological Patterns: A Well-Known
#'   Algorithm from Computer Science Aids the Evaulation of Species
#'   Distributions. \emph{American Scientist}, \strong{88}, pp. 332--339.
#' @name finches
#' @example inst/examples/ex-lpca.r
NULL

#' Multivariate marine biological and environmental data
#' 
#' Species counts and environmental variables for several locations on a
#' sea-bed.
#' 
#' @format A \code{\link[tibble]{tibble}}.
#' @keywords datasets
#' @source Greenacre M.J. (2010). \emph{Biplots in Practice}, pp. 25--26. 
#'   Fundacion BBVA.
#' @name bioenv
#' @example inst/examples/ex-lm.r
NULL

#' Student ratings of similarities and attributes of 13 countries
#' 
#' Greenacre (2010) used these ratings to illustrate multidimensional scaling. 
#' The uses shown there are reproduced in an example.
#' 
#' @format \code{\link[=matrix]{Matrices}}.
#' @keywords datasets
#' @source Greenacre M.J. (2010). \emph{Biplots in Practice}, pp. 43--44,46. 
#'   Fundacion BBVA.
#' @name countries
#' @rdname countries
#' @aliases country_differences country_attributes
#'   
"country_differences"
#' @name countries
#' @rdname countries
#' @usage data(country_differences)
#' @format NULL
"country_attributes"
#' @name countries
#' @rdname countries
#' @usage data(country_attributes)
#' @format NULL
NULL

#' Marine species from sea-bed (benthic) samples
#'
#' Counts of 92 benthic species at 13 sites in the North Sea following an oil
#' exploration.
#'
#' @format A \code{\link[=matrix]{matrix}}.
#' @keywords datasets
#' @source Greenacre M.J. (2010). \emph{Biplots in Practice}. Fundacion BBVA.
#'   \url{https://www.fbbva.es/microsite/multivariate-statistics/data.html}
#' @name benthos
#' @example inst/examples/ex-ca.r
NULL

#' Original respondent-level data from Spanish ISSP sample
#'
#' Responses to several questions about working women and family
#'
#' @format A \code{\link[=matrix]{matrix}}.
#' @keywords datasets
#' @source Greenacre M.J. (2010). \emph{Biplots in Practice}. Fundacion BBVA.
#'   \url{https://www.fbbva.es/microsite/multivariate-statistics/data.html}
#' @name issp_women
#' @example inst/examples/ex-ca.r
NULL
