#' @title Restrict geometric data to boundary points for its convex hull
#'
#' @description As used in a **[ggplot2][ggplot2::ggplot2]** vignette, this stat
#'   layer restricts a dataset with `x` and `y` variables to the points that lie
#'   on its convex hull. The biplot extension restricts each matrix factor to
#'   its own hull.
#'   

#' @template biplot-layers
#' @template biplot-ord-aes

#' @inheritParams ggplot2::layer
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-chull-haireye.r
#' @export
stat_chull <- function(
  mapping = NULL, data = NULL, geom = "polygon", position = "identity",
  show.legend = NA, 
  inherit.aes = TRUE,
  ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatChull,
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

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatChull <- ggproto(
  "StatChull", Stat,
  
  required_aes = c("x", "y"),
  
  compute_group = function(
    data, scales
  ) {
    ord_cols <- get_ord_aes(data)
    
    data[chull(data[, ord_cols, drop = FALSE]), , drop = FALSE]
  }
)



# proportional convex hulls / alpha bags
# adapted from `aplpack::plothulls()`
# https://cran.r-project.org/package=aplpack
cbag <- function(x, y, prop = c(1), cut = c("above", "below")) {
  cut <- match.arg(cut, c("above", "below"))
  n <- length(x)
  prop <- rev(sort(unique(prop)))
  
  # initialize output
  res <- tibble::tibble()
  
  # initial convex hull contains all points
  cut_prop <- length(x) / n
  i_hull <- chull(x, y)
  x_hull <- x[i_hull]; y_hull <- y[i_hull]
  x <- x[-i_hull]; y <- y[-i_hull]
  
  # sequentially obtain proportional hulls
  dupe <- FALSE
  for (i in seq_along(prop)) {
    
    # peel convex hulls until next one drops below `prop[i]`
    while (length(x) / n >= prop[i] && length(x) > 0L) {
      dupe <- FALSE
      cut_prop <- length(x) / n
      i_hull <- chull(x, y)
      x_hull <- x[i_hull]; y_hull <- y[i_hull]
      x <- x[-i_hull]; y <- y[-i_hull]
    }
    # peel last hull to cut below `prop`
    if (cut_prop > prop[i] && cut == "below") {
      dupe <- FALSE
      cut_prop <- length(x) / n
      i_hull <- chull(x, y)
      x_hull <- x[i_hull]; y_hull <- y[i_hull]
      x <- x[-i_hull]; y <- y[-i_hull]
    }
    
    if (! dupe) {
      # append data
      res_p <- tibble::tibble(
        x = x_hull,
        y = y_hull,
        hull = i,
        prop = prop[i],
        cut = cut_prop
      )
      res <- rbind(res, res_p)
    }
    dupe <- TRUE
  }
  
  return(res)
}
