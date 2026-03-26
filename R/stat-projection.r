#' @title Project rows onto columns or vice-versa
#'
#' @description Compute projections of vectors from one matrix factor onto those
#'   of the other.
#' 

#' @details An ordination model of continuous data can be used to predict values
#'   along one dimension from those along the other, using the artificial axes
#'   as intermediaries. The predictions correspond geometrically to projections
#'   of elements of one matrix factor in principal coordinates onto those of the
#'   other factor in standard coordinates. In the most familiar setting of PCA
#'   biplots, variable (column) values are predicted from case (row) locations
#'   along PC1 and PC2. This transformation obtains the axis projections as
#'   `xend,yend` and pairs them with original points `x,y` to demarcate segments
#'   visualizing the projections.
#'
#'   **WARNING:**
#'   This layer is appropriate only with axes in standard coordinates (usually
#'   [`confer_inertia(p = "rows")`][confer_inertia]) and predictive calibration
#'   ([`ggbiplot(axis.type = "predictive")`][ggbiplot]).
#' 

#' @template stat-referent
#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`xend,yend`}{projections onto (specified) vectors}
#' }

#' @importFrom gggda StatReferent
#' @inheritParams ggplot2::layer
#' @inheritParams gggda::stat_referent
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-projection.r
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
                           referent = NULL, na.rm = FALSE) {
    
    # no referent means no projection
    if (is.null(referent) || ! is.data.frame(referent)) return(data.frame())
    
    # extract positions
    ord_cols <- get_ord_aes(data)
    data_ord <- data[, ord_cols, drop = FALSE]
    ref_ord <- referent[, ord_cols, drop = FALSE]
    
    # compute and collect projections of `data` onto `referent` rows
    # (repeat across referent elements within data elements)
    inertia <- rep(rowSums(ref_ord^2), times = nrow(data))
    inner_prod <- as.vector( as.matrix(ref_ord) %*% t(as.matrix(data_ord)) )
    data <- data[rep(seq(nrow(data)), each = nrow(referent)), , drop = FALSE]
    data <- transform(
      data,
      xend = inner_prod / inertia * ref_ord[[1L]],
      yend = inner_prod / inertia * ref_ord[[2L]]
    )
    
    # output segment data
    data
  }
)
