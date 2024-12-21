#' @title Project rows onto columns or vice-versa
#'

#' @description Compute projections of vectors from one matrix factor onto those
#'   of the other.
#' 

#' @details
#' 
#' TODO: Document the projection statistical transformation.
#' 

#' @template biplot-layers
#' @template stat-referent

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`xend,yend`}{projections onto (specified) vectors}
#' }

#' @include stat-referent.r
#' @inheritParams ggplot2::layer
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' 
#' @export
stat_projection <- function(
    mapping = NULL, data = NULL, geom = "segment", position = "identity",
    referent = NULL,
    ...,
    show.legend = NA, inherit.aes = TRUE
) {
  LayerRef <- layer(
    data = data,
    mapping = mapping,
    stat = StatProjection,
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
  
  # undocumented class for custom `ggplot_add()` method
  class(LayerRef) <- c("LayerRef", class(LayerRef))
  LayerRef
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatProjection <- ggproto(
  "StatProjection", StatReferent,
  
  compute_group = function(data, scales, referent = NULL, na.rm = FALSE) {
    
    # arbitrary values of computed aesthetics
    res <- transform(
      data,
      xend = NA_real_,
      yend = NA_real_
    )
    # empty initialized output
    res <- data[c(), , drop = FALSE]
    
    # no referent means no projection
    if (is.null(referent) || ! is.data.frame(referent)) return(res)
    
    # compute and collect projections of `data` onto `referent` rows
    inertias <- referent$x^2 + referent$y^2
    for (i in seq(nrow(referent))) {
      data$dots <- data$x * referent$x[i] + data$y * referent$y[i]
      res_i <- transform(
        data,
        xend = dots / inertias[i] * referent$x[i],
        yend = dots / inertias[i] * referent$y[i]
      )
      res <- rbind(res, res_i)
    }
    
    # output segment data
    res
  }
)
