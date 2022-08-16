#' @title Restrict geometric data to boundary points for its conical hull
#'
#' @description This stat layer restricts a dataset with `x` and `y` variables
#'   to the points that lie on its conical hull (other than the origin).
#'   

#' @template biplot-layers
#' @template biplot-ord-aes

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param origin Logical; whether to include the origin with the transformed
#'   data. Defaults to `FALSE`.
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-chull-spend.r
#' @export
stat_cone <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  origin = FALSE,
  show.legend = NA, 
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatCone,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      na.rm = FALSE,
      origin = origin,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatCone <- ggproto(
  "StatCone", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(
    data, scales,
    origin = FALSE
  ) {
    ord_cols <- get_ord_aes(data)
    
    # if the data set contains the origin, then the convex hull suffices
    if (any(apply(as.matrix(data[, ord_cols, drop = FALSE]) == 0, 1L, all))) {
      return(data[chull(data$x, data$y), , drop = FALSE])
    }
    
    # append the origin to the data set for the convex hull calculation
    hull_data <- rbind(data[, ord_cols, drop = FALSE], rep(0, length(ord_cols)))
    hull <- chull(hull_data)
    # if the new origin is not in the convex hull, then the convex hull suffices
    orig <- match(nrow(data) + 1L, hull)
    if (is.na(orig)) return(data[hull, , drop = FALSE])
    
    # cycle the rows of the hull until the origin is first
    hull <- c(hull[seq(orig, length(hull))], hull[seq(0L, orig - 1L)[-1L]])
    # if origin is to be omitted, return the convex hull from the data
    if (! origin) return(data[hull[-1L], , drop = FALSE])
    
    # reduce additional columns: unique or bust
    data_only <- as.data.frame(lapply(subset(data, select = -ord_cols), only))
    # bind additional columns to origin
    data_orig <- hull_data[nrow(hull_data), , drop = FALSE]
    if (ncol(data_only) > 0) data_orig <- merge(data_orig, data_only)
    # append the origin data to the input data
    data <- rbind(data, data_orig)
    # return the convex hull
    data[hull, , drop = FALSE]
  }
)

# single unique value, or else NA
only <- function(x) {
  uniq <- unique(x)
  if (length(uniq) == 1L) {
    uniq
  } else {
    switch(
      class(x),
      integer = NA_integer_,
      numeric = NA_real_,
      character = NA_character_,
      factor = factor(NA_character_, levels = levels(x))
    )
  }
}
