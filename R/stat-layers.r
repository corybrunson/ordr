#' Render plot elements for one matrix of an ordination
#' 

#' @template ggbiplot-layers

#' @name ggbiplot-matrix
#' @inheritParams ggplot2::layer
#' @param ... Additional arguments passed to \code{\link[ggplot2]{layer}}.

#' @rdname ggbiplot-matrix
#' @usage NULL
#' @export
StatU <- ggproto(
  "StatU", StatIdentity,
  
  setup_data = function(data, params) {
    data[data$.matrix == "u", -match(".matrix", names(data))]
  }
)

#' @rdname ggbiplot-matrix
#' @usage NULL
#' @export
StatV <- ggproto(
  "StatV", StatIdentity,
  
  setup_data = function(data, params) {
    data[data$.matrix == "v", -match(".matrix", names(data))]
  }
)

#' @rdname ggbiplot-matrix
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

#' @rdname ggbiplot-matrix
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
