#' @title **ordr** package
#'
#' @description This is a **[tidyverse][tidyverse::tidyverse]** extension for
#'   handling, manipulating, and visualizing ordination models with consistent
#'   conventions and in a tidy workflow.
#'   

#' @details
#'
#' This package is designed to integrate ordination analysis and biplot
#' visualization into a **tidyverse** workflow. It is inspired in particular by
#' the extensions **ggbiplot** and **tidygraph**.
#'

#' The package consists in several modules:
#' - the `"tbl_ord"` class, a wrapper for various ordination object classes
#' - extracting [augmentation] for the factors of an ordination
#' - using [dplyr-verbs] to add [annotation] to the factors of an ordination
#' - manipulating the coordinates of an ordination via [alignment] with other
#' objects and [conference] of inertia
#' - methods of the above generics for several widely-used ordination object
#' classes
#' - convenient [formatting] of ordination objects that respects the above
#' manipulations
#' - [ggbiplot], a **ggplot2** extension for rendering biplots, including
#' several plot layers, integrated with the `"tbl_ord"` class

#' @docType package
#' @name ordr
NULL

if (getRversion() >= "2.15.1") utils::globalVariables(c(
  "."
))
