#' @title Convex hulls and hull peelings
#'
#' @description Restrict planar data to the boundary points of its convex hull,
#'   or of nested convex hulls containing specified fractions of points.
#'

#' @details As used in a **[ggplot2][ggplot2::ggplot2]** vignette,
#'   `stat_chull()` restricts a dataset with `x` and `y` variables to the points
#'   that lie on its convex hull.
#'
#'   Building on this, `stat_peel()` returns hulls from a _convex hull peeling_:
#'   a subset of sequentially removed hulls containing specified fractions of
#'   the data.
#' 

#' @template ref-barnett1976

#' @template biplot-layers
#' @template biplot-ord-aes

#' @section Computed variables: These are calculated during the statistical
#'   transformation and can be accessed with [delayed
#'   evaluation][ggplot2::aes_eval].
#' \describe{
#'   \item{`hull`}{the position of `breaks` that defines each hull}
#'   \item{`frac`}{the value of `breaks` that defines each hull}
#'   \item{`prop`}{the actual proportion of data within each hull}
#' }

#' @importFrom grDevices chull
#' @inheritParams ggplot2::layer
#' @param breaks A numeric vector of fractions (between `0` and `1`) of the
#'   data to contain in each hull.
#' @param cut Character; one of `"above"` and `"below"`, indicating whether each
#'   hull should contain at least or at most `breaks` of the data,
#'   respectively.
#' @template param-stat
#' @template return-layer
#' @family stat layers
#' @example inst/examples/ex-stat-chull.r
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
stat_peel <- function(
    mapping = NULL, data = NULL, geom = "polygon", position = "identity",
    breaks = c(.5), cut = c("above", "below"),
    show.legend = NA, 
    inherit.aes = TRUE,
    ...
) {
  layer(
    data = data,
    mapping = mapping,
    stat = StatPeel,
    geom = geom, 
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      breaks = breaks,
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
StatPeel <- ggproto(
  "StatPeel", StatChull,
  
  compute_group = function(
    data, scales,
    breaks = c(.5), cut = c("above", "below")
  ) {
    
    ord_cols <- get_ord_aes(data)
    
    peel_data <- peel_hulls(
      data[, ord_cols, drop = FALSE],
      breaks = breaks, cut = cut
    )
    
    # interact existing group with hull
    peel_data$group <- 
      interaction(unique(data$group), peel_data$hull, lex.order = TRUE)
    
    peel_data
  }
)

# convex hull peelings
# adapted from `aplpack::plothulls()`
# https://cran.r-project.org/package=aplpack
peel_hulls <- function(
    data, breaks = c(.5), cut = c("above", "below"), nonempty = FALSE
) {
  
  # behave like `chull()`: assume first two columns are `x` and `y`
  x <- data[[1L]]; y <- data[[2L]]
  xy <- names(data)[seq(2L)]
  n <- nrow(data)
  breaks <- rev(sort(unique(breaks)))
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
  for (i in seq_along(breaks)) {
    
    # peel convex hulls until next one drops below `breaks[i]`
    while (length(x) / n >= breaks[i] && length(x) > 0L) {
      dupe <- FALSE
      cut_prop <- length(x) / n
      i_hull <- chull(x, y)
      x_hull <- x[i_hull]; y_hull <- y[i_hull]
      x <- x[-i_hull]; y <- y[-i_hull]
    }
    # peel last hull to cut below `breaks`
    if (cut_prop > breaks[i] && cut == "below") {
      dupe <- FALSE
      cut_prop <- length(x) / n
      i_hull <- chull(x, y)
      if (length(i_hull) == 0L && nonempty) break
      x_hull <- x[i_hull]; y_hull <- y[i_hull]
      x <- x[-i_hull]; y <- y[-i_hull]
    }
    
    if (! dupe) {
      # append data
      res_p <- tibble::tibble(
        x = x_hull,
        y = y_hull,
        hull = i,
        frac = breaks[i],
        prop = cut_prop
      )
      res <- rbind(res, res_p)
    }
    dupe <- TRUE
  }
  
  names(res)[seq(2L)] <- xy
  res$hull <- factor(res$hull, levels = seq_along(breaks))
  return(res)
}
