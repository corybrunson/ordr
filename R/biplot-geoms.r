#' @title Convenience geoms for row and column matrix factors
#'
#' @description These geometric element layers (geoms) pair conventional
#'   **ggplot2** geoms with [stat_rows()] or [stat_cols()] in order to render
#'   elements for one or the other matrix factor of a tbl_ord. They understand
#'   the same aesthetics as their corresponding conventional geoms.
#'
#' @name biplot-geoms
#' @import ggplot2
#' @importFrom ggrepel GeomTextRepel GeomLabelRepel
#' @inheritParams ggplot2::layer
#' @template param-matrix
#' @template param-geom
#' @inheritParams ggplot2::geom_text
#' @inheritParams geom_axis
#' @inheritParams geom_isolines
#' @inheritParams geom_vector
#' @family biplot layers
NULL
