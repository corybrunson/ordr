#' @title Calculate a minimum spanning tree among cases or variables
#'
#' @description This stat layer identifies the \eqn{n-1} pairs among \eqn{n}
#'   points that form a minimum spanning tree, then calculates the segments
#'   between these poirs in the two dimensions `x` and `y`.
#'   

#' @details
#' 
#' A minimum spanning tree (MST) on the point cloud \eqn{X} is a minimal
#' connected graph on \eqn{X} with the smallest possible sum of distances (or
#' dissimilarities) between linked points. These layers call [stats::dist()] to
#' calculate a distance/dissimilarity object and [vegan::spantree()] to
#' calculate the MST. The result is formatted with position aesthetics readable
#' by [ggplot2::geom_segment()].
#'
#' An MST calculated on `x` and `y` reflects the distances among the points in
#' \eqn{X} in the reduced-dimension plane of the biplot. In contrast, one
#' calculated on the full set of coordinates reflects distances in
#' higher-dimensional space. Plotting this high-dimensional MST on the
#' 2-dimensional biplot provides a visual cue as to how faithfully two
#' dimensions can encapsulate the "true" distances between points (Jolliffe,
#' 2002).
#' 

#' @template ref-jolliffe2002
#'   

#' @template biplot-layers
#' @template biplot-ord-aes

#' @inheritParams ggplot2::layer
#' @param method Passed to [stats::dist()].
#' @template param-stat
#' @family stat layers
#' @export
stat_spantree <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  method = "euclidean",
  show.legend = NA, inherit.aes = TRUE, check.aes = TRUE, check.param = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatSpantree,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    check.param = check.param,
    params = list(
      method = method,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatSpantree <- ggproto(
  "StatSpantree", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           method = "euclidean") {
    ord_cols <- get_ord_aes(data)
    
    # distance/dissimilarity data
    data_dis <- dist(data[, ord_cols, drop = FALSE], method = method)
    # minimum spanning tree
    data_mst <- vegan::spantree(data_dis)
    # pairs of linked points
    links <- cbind(2:attr(data_dis, "Size"), data_mst$kid)
    links <- links[! is.na(links[, 1]) & ! is.na(links[, 2]), , drop = FALSE]
    
    # data frame of segment coordinates
    segments <- data.frame(
      x = data[links[, 1], "x"], xend = data[links[, 2], "x"],
      y = data[links[, 1], "y"], yend = data[links[, 2], "y"]
    )
    
    # bind other columns back in (by parents)
    data <- merge(
      segments, data, by = c("x", "y"),
      all.x = TRUE, all.y = FALSE
    )
    
    data
  }
)
