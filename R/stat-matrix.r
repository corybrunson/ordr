#' @title Render plot elements for one matrix of an ordination
#'
#' @description These stats merely tell [ggplot2::ggplot()] which factor of an
#'   ordination to pull data from for a plot layer. They are invoked internally
#'   by the various `geom_*_*()` layers.
#'   

#' @template biplot-layers

#' @name biplot-matrix
#' @inheritParams ggplot2::layer
#' @template param-stat
NULL

#' @rdname biplot-matrix
#' @usage NULL
#' @export
StatU <- ggproto(
  "StatU", StatIdentity,
  
  setup_data = setup_u_data
)

#' @rdname biplot-matrix
#' @usage NULL
#' @export
StatV <- ggproto(
  "StatV", StatIdentity,
  
  setup_data = setup_v_data
)

#' @rdname biplot-matrix
#' @export
stat_u <- function(
  mapping = NULL, data = data,
  geom = "point", position = "identity",
  ...,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "u",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname biplot-matrix
#' @export
stat_v <- function(
  mapping = NULL, data = data,
  geom = "axis", position = "identity",
  ...,
  show.legend = NA, inherit.aes = TRUE
) {
  layer(
    data = data,
    mapping = mapping,
    stat = "v",
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      ...
    )
  )
}
