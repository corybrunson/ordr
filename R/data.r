
#' @title Site-species occurrence data for Galapagos finches
#'
#' @format A [data.frame][base::data.frame].
#' @keywords datasets
#' @source These data are taken from Sanderson (2000). They are available in a
#'   different format in the **[cooccur][cooccur::cooccur]** package.
#' @template ref-sanderson2000
#' @name finches
#' @example inst/examples/ex-lpca.r
NULL

#' @title Multivariate marine biological and environmental data
#'
#' @description Species counts and environmental variables for several locations
#'   on a sea-bed.
#'
#' @format A [tibble][tibble::tibble].
#' @keywords datasets
#' @source Greenacre (2010), pp. 25--26.
#' @template ref-greenacre2010
#' @name bioenv
#' @example inst/examples/ex-lm.r
NULL

#' @title Student ratings of similarities and attributes of 13 countries
#'
#' @description Greenacre (2010) used these ratings to illustrate
#'   multidimensional scaling. The uses shown there are reproduced in an
#'   example.
#'
#' @format [Matrices][base::matrix].
#' @keywords datasets
#' @source Greenacre (2010), pp. 43--44,46.
#' @template ref-greenacre2010
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

#' @title Marine species from sea-bed (benthic) samples
#'
#' @description Counts of 92 benthic species at 13 sites in the North Sea
#'   following an oil exploration.
#'
#' @format A [matrix][base::matrix].
#' @keywords datasets
#' @source Greenacre (2010).
#' @template ref-greenacre2010
#' @name benthos
#' @example inst/examples/ex-ca.r
NULL

#' @title Original respondent-level data from Spanish ISSP sample
#'
#' @description Responses to several questions about working women and family.
#'
#' @format A [matrix][base::matrix].
#' @keywords datasets
#' @source Greenacre (2010).
#' @name issp_women
#' @example inst/examples/ex-ca.r
NULL
