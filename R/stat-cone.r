#' @title Restrict geometric data to boundary points for its conical hull
#'
#' @description This stat layer restricts a dataset with `x` and `y` variables
#'   to the points that lie on its conical hull (other than the origin).
#'   

#' @template biplot-layers

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @param origin Logical; whether to include the origin with the transformed
#'   data. Defaults to `FALSE`.
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

#' @rdname biplot-stats
#' @export
stat_rows_cone <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  origin = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatRowsCone,
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

#' @rdname biplot-stats
#' @export
stat_cols_cone <- function(
  mapping = NULL, data = NULL, geom = "path", position = "identity",
  origin = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatColsCone,
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
    
    # if the data set contains the origin, then the convex hull suffices
    if (any(vapply(data$x == 0 & data$y == 0, isTRUE, TRUE))) {
      return(data[chull(data$x, data$y), , drop = FALSE])
    }
    
    # append the origin to the data set for the convex hull calculation
    chull_data <- data.frame(x = c(data$x, 0), y = c(data$y, 0))
    hull <- chull(chull_data)
    # if the origin is not in the convex hull, then the convex hull suffices
    orig <- match(nrow(data) + 1L, hull)
    if (is.na(orig)) return(data[hull, , drop = FALSE])
    
    # cycle the rows of the hull until the origin is first
    hull <- c(hull[seq(orig, length(hull))], hull[seq(0L, orig - 1L)[-1L]])
    # if origin is to be omitted, return the convex hull from the data
    if (! origin) return(data[hull[-1L], , drop = FALSE])
    
    # reduce additional columns: unique or bust
    data_only <- as.data.frame(lapply(subset(data, select = -c(x, y)), only))
    # bind additional columns to origin
    data_orig <- merge(data.frame(x = 0, y = 0), data_only)
    # append the origin data to the input data
    data <- rbind(data, data_orig)
    # return the convex hull
    data[hull, , drop = FALSE]
  }
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatRowsCone <- ggproto(
  "StatRowsCone", StatCone,
  
  setup_data = setup_rows_data
)

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatColsCone <- ggproto(
  "StatColsCone", StatCone,
  
  setup_data = setup_cols_data
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
