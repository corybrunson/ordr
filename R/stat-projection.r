#' @title Project rows onto columns or vice-versa
#'

#' @description Compute projections of vectors from one matrix factor onto those
#'   of the other.
#' 

#' @details
#'
#' An ordination model of continuous data can be used to predict values along
#' one dimension from those along the other, using the artificial axes as
#' intermediaries. The predictions correspond geometrically to projections of
#' elements of one matrix factor in principal coordinates onto those of the
#' other factor in standard coordinates. In the most familiar setting of PCA
#' biplots, variable (column) values are predicted from case (row) locations
#' along PC1 and PC2. This transformation obtains the axis projections as
#' `xend,yend` and pairs them with original points `x,y` to demarcate segments
#' visualizing the projections.
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
#' @inheritParams stat_referent
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-projection.r
#' @example inst/examples/ex-stat-projection-iris.r
#' @export
stat_projection <- function(
    mapping = NULL, data = NULL, geom = "segment", position = "identity",
    subset = NULL,
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
  class(LayerRef) <- c("LayerRef", class(LayerRef))
  LayerRef
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatProjection <- ggproto(
  "StatProjection", StatReferent,
  
  compute_group = function(data, scales,
                           subset = NULL, referent = NULL, na.rm = FALSE) {
    
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
