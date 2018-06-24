#' Site-species occurrence data for finches on the Galapagos islands
#' 
#' These data are taken from Sanderson (2000). They are available in a different
#' format in the \strong{\link[cooccur]{cooccur}} package.
#' 
#' @format A \link[tibble]{tibble}.
#' @keywords datasets
#' @source Sanderson J.G. (2000). Testing Ecological Patterns: A Well-Known
#'   Algorithm from Computer Science Aids the Evaulation of Species
#'   Distributions. \emph{American Scientist}, \strong{88}, pp. 332--339.
#' @name finches
#' @example inst/examples/ex-lpca.r
NULL

#' A simple illustrative biplot example from Greenacre (2010)
#' 
#' This is a simple example of a biplot, a \eqn{5\times 4} matrix that
#' decomposes into the product of a \eqn{5\times 2} matrix and a \eqn{2\times 4}
#' matrix.
#' 
#' @format A \code{\link{tbl_ord}}.
#' @keywords datasets
#' @source Greenacre M.J. (2010). \emph{Biplots in Practice}, pp. 16--20. 
#'   Fundacion BBVA.
#' @name simple_example
NULL

#' Multivariate marine biological and environmental data
#' 
#' Species counts and environmental variables for several locations on a
#' sea-bed.
#' 
#' @format A \link{tibble}.
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
#' @format \link[tibble]{tibble}s.
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
