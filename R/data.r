#' @title Glass composition data
#'
#' @description Sites, types, and compositions of glass samples from
#'   archaeological sites in Israel.
#'
#' @details
#'
#' Chunks of unformed glass from several furnaces found at the primary
#' Byzantine-era site of Bet Eli'ezer, along with samples from other sites with
#' weaker evidence of glass-making (Apollonia and Dor) and and from an
#' Islamic-era site (Banias), were analyzed using X-ray spectrometry to
#' determine their major components.
#'
#' Baxter & Freestone (2006) used these data to illustrate [log-ratio
#' analysis][lra-ord].
#'
#' @name glass
#' @usage data(glass)
#' @keywords datasets
#' @source Freestone &al (2000), Table 2.
#' @template ref-freestone2000
#' @template ref-baxter2006
#' @format A [tibble][tibble::tibble] with 68 cases and 16 variables:

#' \describe{
#'   \item{Site}{site at which sample was found}
#'   \item{Anal}{analysis identifier}
#'   \item{Context}{furnace identifier}
#'   \item{Form}{type of sample}
#'   \item{SiO2, TiO2, Al2O3, FeO, MnO, MgO, CaO, Na2O, K2O, P2O5, Cl, SO3}{
#'         normalized weight percent oxide of each component}
#' }

#' @example inst/examples/ex-data-glass.r
NULL

#' @title U.S. university rankings
#'
#' @description Classifications and rankings of U.S. universities for the years
#'   2017--2020.
#'
#' @details
#'
#' Ranking data were obtained from the public QS website.
#'
#' @name qswur_usa
#' @usage data(qswur_usa)
#' @keywords datasets
#' @source Quacquarelli Symonds (2021).
#' @template ref-qs2021
#' @format A [tibble][tibble::tibble] of 13 variables on 612 cases:

#' \describe{
#'   \item{year}{year of rankings}
#'   \item{institution}{institution of higher learning}
#'   \item{size}{size category of institution}
#'   \item{focus}{subject range of institution}
#'   \item{res}{research intensity of institution}
#'   \item{age}{age classification of institution}
#'   \item{status}{status of institution}
#'   \item{rk_academic}{rank by academic reputation}
#'   \item{rk_employer}{rank by employer reputation}
#'   \item{rk_ratio}{rank by faculty--student ratio}
#'   \item{rk_citations}{rank by citations per faculty}
#'   \item{rk_intl_faculty}{rank by international faculty ratio}
#'   \item{rk_intl_students}{rank by international student ratio}
#' }

#' @example inst/examples/ex-data-qswur.r
NULL
