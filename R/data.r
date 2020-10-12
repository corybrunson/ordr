
#' @title Site-species occurrence data for Galapagos finches
#'
#' @format A [data frame][base::data.frame].
#' @keywords datasets
#' @source These data are taken from Sanderson (2000). They are available in a
#'   different format in the **[cooccur][cooccur::cooccur]** package.
#' @template ref-sanderson2000
#' @name finches
#' @usage data(finches)
#' @example inst/examples/finches-ca.r
#' @example inst/examples/finches-lpca-secondary.r
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
#' @usage data(bioenv)
#' @example inst/examples/bioenv-lm-isolines.r
#' @example inst/examples/bioenv-cmds-lm-repel.r
#' @example inst/examples/bioenv-mlm.r
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
#' @aliases country_differences country_attributes
#' @usage data(country_differences)
#' @usage data(country_attributes)
#' @example inst/examples/country-cmds-lm.r
#' @example inst/examples/country-prcomp-confer.r
#' @example inst/examples/country-cmds-prcomp-negate.r
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
#' @usage data(benthos)
#' @example inst/examples/benthos-ca-augment-confer.r
NULL

#' @title Original respondent-level data from Spanish ISSP sample
#'
#' @description Responses to several questions about working women and family.
#'
#' @format A [matrix][base::matrix].
#' @keywords datasets
#' @source Greenacre (2010).
#' @template ref-greenacre2010
#' @name issp_women
#' @usage data(issp_women)
#' @example inst/examples/women-ca-confer.r
NULL

#' @title Glass composition data from archaeological sites in Israel
#'
#' @description Sites, artifacts, and amounts of several components of the sand.
#'
#' @format A [tibble][tibble::tibble].
#' @keywords datasets
#' @source Baxter & Freestone (2006).
#' @name glass
#' @usage data(glass)
#' @example inst/examples/glass-pca-lda-secondary.r
NULL

#' @title Ingredient ratios for several basic recipes
#'
#' @description Ratios of ingredients for several basic recipes presented in a
#'   popular cooking book.
#'
#' @format A [tibble][tibble::tibble].
#' @keywords datasets
#' @source Ruhlman (2009).
#' @template ref-ruhlman2009
#' @name ratios
#' @usage data(ratios)
#' @example inst/examples/ratios-ca-verbs.r
NULL
