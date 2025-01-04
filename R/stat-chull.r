#' @title Convex hulls and alpha bags
#'
#' @description Restrict planar data to the boundary points of its convex hull,
#'   or of nested convex hulls containing specified fractions of points.
#'

#' @description As used in a **[ggplot2][ggplot2::ggplot2]** vignette,
#'   `stat_chull()` restricts a dataset with `x` and `y` variables to the points
#'   that lie on its convex hull.
#'
#'   Building on this, `stat_bag()` returns an alpha bag: a subset of
#'   sequentially peeled hulls containing specified fractions of the data.
#' 

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`hull`}{the position of `fraction` that defines each hull}
#'   \item{`frac`}{the value of `fraction` that defines each hull}
#'   \item{`prop`}{the actual proportion of data within each hull}
#' }

#' @importFrom grDevices chull
#' @inheritParams ggplot2::layer
#' @param fraction A numeric vector of fractions (between `0` and `1`) of the
#'   data to contain in each hull.
#' @param cut Character; one of `"above"` and `"below"`, indicating whether each
#'   hull should contain at least or at most `fraction` of the data,
#'   respectively.
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-chull-haireye.r
#' @example inst/examples/ex-stat-bag-judges.r
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

#' @rdname stat_chull
#' @export
stat_bag <- function(
    mapping = NULL, data = NULL, geom = "polygon", position = "identity",
    fraction = c(.5), cut = c("above", "below"),
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatBag,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      fraction = fraction,
      cut = cut,
      na.rm = FALSE,
      ...
    )
  )
}

#' @rdname ordr-ggproto
#' @format NULL
#' @usage NULL
#' @export
StatBag <- ggproto(
  "StatBag", StatChull,
  
  compute_group = function(
    data, scales,
    fraction = c(.5), cut = c("above", "below")
  ) {
    
    ord_cols <- get_ord_aes(data)
    
    bag_data <- compute_bag(
      data[, ord_cols, drop = FALSE],
      fraction = fraction, cut = cut
    )
    
    # interact existing group with hull
    bag_data$group <- 
      interaction(unique(data$group), bag_data$hull, lex.order = TRUE)
    
    bag_data
  }
)

# proportional convex hulls / alpha bags
# adapted from `aplpack::plothulls()`
# https://cran.r-project.org/package=aplpack
compute_bag <- function(data, fraction = c(.5), cut = c("above", "below")) {
  
  # behave like `chull()`: assume first two columns are `x` and `y`
  x <- data[[1L]]; y <- data[[2L]]
  xy <- names(data)[seq(2L)]
  n <- nrow(data)
  fraction <- rev(sort(unique(fraction)))
  cut <- match.arg(cut, c("above", "below"))
  
  # initialize output
  res <- tibble::tibble()
  
  # initial convex hull contains all points
  cut_prop <- length(x) / n
  i_hull <- chull(x, y)
  x_hull <- x[i_hull]; y_hull <- y[i_hull]
  x <- x[-i_hull]; y <- y[-i_hull]
  
  # sequentially obtain proportional hulls
  dupe <- FALSE
  for (i in seq_along(fraction)) {
    
    # peel convex hulls until next one drops below `fraction[i]`
    while (length(x) / n >= fraction[i] && length(x) > 0L) {
      dupe <- FALSE
      cut_prop <- length(x) / n
      i_hull <- chull(x, y)
      x_hull <- x[i_hull]; y_hull <- y[i_hull]
      x <- x[-i_hull]; y <- y[-i_hull]
    }
    # peel last hull to cut below `fraction`
    if (cut_prop > fraction[i] && cut == "below") {
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
        frac = fraction[i],
        prop = cut_prop
      )
      res <- rbind(res, res_p)
    }
    dupe <- TRUE
  }
  
  names(res)[seq(2L)] <- xy
  res$hull <- factor(res$hull, levels = seq_along(fraction))
  return(res)
}
