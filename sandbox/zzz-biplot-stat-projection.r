
#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsProjection <- ggproto(
  "StatRowsProjection", StatProjection,
  
  setup_params = setup_referent_params,
  
  setup_data = setup_rows_xy_data
)

#' @rdname biplot-stats
#' @export
stat_rows_projection <- function(
    mapping = NULL,
    data = NULL,
    geom = "segment",
    position = "identity",
    referent = NULL,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  LayerRef <- layer(
    data = data,
    mapping = mapping,
    stat = StatRowsProjection,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      referent = referent,
      na.rm = FALSE,
      ...
    )
  )
  class(LayerRef) <- c("LayerRef", class(LayerRef))
  LayerRef
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsProjection <- ggproto(
  "StatColsProjection", StatProjection,
  
  setup_params = setup_referent_params,
  
  setup_data = setup_cols_xy_data
)

#' @rdname biplot-stats
#' @export
stat_cols_projection <- function(
    mapping = NULL,
    data = NULL,
    geom = "segment",
    position = "identity",
    referent = NULL,
    show.legend = NA,
    inherit.aes = TRUE,
    ...
) {
  LayerRef <- layer(
    data = data,
    mapping = mapping,
    stat = StatColsProjection,
    geom = geom,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      referent = referent,
      na.rm = FALSE,
      ...
    )
  )
  class(LayerRef) <- c("LayerRef", class(LayerRef))
  LayerRef
}
