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
#' If any aesthetics of the form `.coord[0-9]+` are detected, then the lot of
#' them are used to calculate distances/dissimilarities. These should not be
#' assigend manually but generated using the convenience function [ord_aes()]
#' (see the examples). Otherwise, `x` and `y` are used. Either way, `x` and `y`
#' provide the position aesthetics.
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

#' @template ggbiplot-layers

#' @name ggbiplot-spantree
#' @inheritParams ggplot2::layer
#' @param method Passed to [stats::dist()].
#' @template param-stat
#' @example inst/examples/ex-spantree.r
NULL

#' @rdname ggbiplot-spantree
#' @usage NULL
#' @export
StatSpantree <- ggproto(
  "StatSpantree", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(data, scales,
                           method = "euclidean") {
    
    # columns to use in distance/dissimilarity calculation
    dis_cols <- grep("^\\.coord[0-9]+$", names(data))
    if (length(dis_cols) == 0) dis_cols <- match(c("x", "y"), names(data))
    #if (is.null(data$coord)) dis_cols <- c("x", "y") else dis_cols <- "coord"
    
    # distance/dissimilarity data
    data_dis <- dist(data[, dis_cols, drop = FALSE], method = method)
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

#' @rdname ggbiplot-spantree
#' @export
stat_spantree <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  method = "euclidean",
  show.legend = NA, inherit.aes = TRUE, check.aes = TRUE,
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
    params = list(
      method = method,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-spantree
#' @usage NULL
#' @export
StatUSpantree <- ggproto(
  "StatUSpantree", StatSpantree,
  
  setup_data = setup_u_data
)

#' @rdname ggbiplot-spantree
#' @usage NULL
#' @export
StatVSpantree <- ggproto(
  "StatVSpantree", StatSpantree,
  
  setup_data = setup_v_data
)

#' @rdname ggbiplot-spantree
#' @export
stat_u_spantree <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  method = "euclidean",
  show.legend = NA, inherit.aes = TRUE, check.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatUSpantree,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    params = list(
      method = method,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ggbiplot-spantree
#' @export
stat_v_spantree <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  method = "euclidean",
  show.legend = NA, inherit.aes = TRUE, check.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatVSpantree,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    check.aes = check.aes,
    params = list(
      method = method,
      na.rm = FALSE,
      ...
    )
  )
}
