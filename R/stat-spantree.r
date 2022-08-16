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
#' @param engine A single character string specifying the package implementation
#'   to use; either `"vegan"` or `"mlpack"`.
#' @param method Passed to [stats::dist()] if `engine` is `"vegan"`, ignored if
#'   `"mlpack"`.
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-spantree-eurodist.r
#' @export
stat_spantree <- function(
  mapping = NULL, data = NULL, geom = "segment", position = "identity",
  engine = "vegan", method = "euclidean",
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
      engine = engine, method = method,
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
                           engine = "vegan", method = "euclidean") {
    data_ord <- data[, get_ord_aes(data), drop = FALSE]
    
    # minimum spanning tree engine
    mst_engines <- c("vegan", "mlpack")
    all_pkgs <- .packages(all.available = TRUE)
    engine <- match.arg(engine, mst_engines)
    if (! engine %in% all_pkgs) {
      engine_alt <- mst_engines[which(mst_engines %in% all_pkgs)[[1L]]]
      if (length(engine_alt) == 1L) {
        warning("Package {", engine, "} not installed; ",
                "using {", engine_alt, "} instead.")
        engine <- engine_alt
      } else {
        stop("No spantree engine installed; requires one of the following:\n",
             "{", paste(mst_engines, collapse = "}, {"), "}")
      }
    }
    
    # minimum spanning tree calculation
    links <- switch(
      engine,
      vegan = {
        # distance/dissimilarity data
        data_dist <- dist(data_ord, method = method)
        # minimum spanning tree
        data_mst <- vegan::spantree(data_dist)
        # pairs of linked points
        links <- cbind(2:attr(data_dist, "Size"), data_mst$kid)
        links[! is.na(links[, 1]) & ! is.na(links[, 2]), , drop = FALSE]
      },
      mlpack = {
        if (method != "euclidean") {
          warning("{", engine, "} engine uses the euclidean distance; ",
                  "`method` will be ignored.")
        }
        # minimum spanning tree (euclidean distance)
        data_emst <- mlpack::emst(data_ord)
        # re-index pairs
        data_emst$output[, 1:2] + 1L
      }
    )
    
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
