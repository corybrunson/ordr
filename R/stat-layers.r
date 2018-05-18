
#' @rdname stat-layers
#' @export
StatU <- ggproto(
  "StatU", StatIdentity,
  
  setup_data = function(data, params) {
    data[data$.matrix == "u", -match(".matrix", names(data))]
  }
)

#' @rdname stat-layers
#' @export
StatV <- ggproto(
  "StatV", StatIdentity,
  
  setup_data = function(data, params) {
    data[data$.matrix == "v", -match(".matrix", names(data))]
  }
)

#' @rdname stat-layers
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

#' @rdname stat-layers
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
