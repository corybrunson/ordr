
#' A synchronization flag.
#'
#' The inert function `sync()` operates analogously to [ggplot2::waiver()] to
#' indicate that an auxiliary aesthetic should be synchronized to a standard
#' aesthetic (when it is mapped from data). `is.sync()` reports whether an
#' object is of this class.
#'
#' @export
#' @keywords internal
sync <- function() structure(list(), class = "sync")

#' @param x An object to test
#' @export
#' @rdname sync
is.sync <- function(x) inherits(x, "sync")

# sensible default for biplot arrows
default_arrow <- grid::arrow(
  angle = 30,
  length = unit(.02, "native"),
  ends = "last",
  type = "open"
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

# The following functions recover certain geometric variables from others.
# They are called by `GeomAxis` and `GeomRule` for the following reasons:
# * To allow the user to pass different kinds of data.
# * To hide position aesthetics during the plotting window calculation.

# introduce `x` & `y` if passed only `angle` & `radius` and vice-versa
ensure_cartesian_polar <- function(data) {
  if ((is.null(data[["x"]]) || is.null(data[["y"]])) && 
      (is.null(data[["angle"]]) || is.null(data[["radius"]])))
    stop("This step requires either `x` and `y` or `angle` and `radius`.")
  
  if (is.null(data[["angle"]])) data$angle <- with(data, atan2(y, x))
  if (is.null(data[["radius"]])) data$radius <- with(data, sqrt(x^2 + y^2))
  if (is.null(data[["x"]])) data$x <- with(data, radius * cos(angle))
  if (is.null(data[["y"]])) data$y <- with(data, radius * sin(angle))
  
  data
}

recover_offset_endpoints <- function(data) {
  # requires angle, yintercept, xintercept
  # computes xend, yend
  
  if (is.null(data[["yintercept"]]) && ! is.null(data[["xintercept"]])) {
    offset <- with(data, xintercept * cos(angle + pi/2))
    data$yintercept <- with(data, offset / sin(angle + pi/2))
  } else if (! is.null(data[["yintercept"]]) && is.null(data[["xintercept"]])) {
    offset <- with(data, yintercept * sin(angle + pi/2))
    data$xintercept <- with(data, offset / cos(angle + pi/2))
  } else if (! is.null(data[["yintercept"]]) &&
             ! is.null(data[["xintercept"]])) {
    # use more accurate intercept (closer to origin)
    offset <- ifelse(
      with(data, yintercept <= xintercept),
      with(data, yintercept * sin(angle + pi/2)),
           with(data, xintercept * cos(angle + pi/2))
    )
  }
  
  if (is.null(data[["xend"]]) || is.null(data[["yend"]])) {
    # offset coordinates expand window to normal in case no rule is computed
    data$xend <- with(data, offset * cos(angle + pi/2))
    data$yend <- with(data, offset * sin(angle + pi/2))
  }
  
  data
}

recover_offset_intercepts <- function(data) {
  # requires angle, xend, yend
  # computes yintercept, xintercept
  
  if (is.null(data[["yintercept"]]) && ! is.null(data[["xintercept"]])) {
    offset <- with(data, xintercept * cos(angle + pi/2))
    data$yintercept <- with(data, offset / sin(angle + pi/2))
  } else if (! is.null(data[["yintercept"]]) && is.null(data[["xintercept"]])) {
    offset <- with(data, yintercept * sin(angle + pi/2))
    data$xintercept <- with(data, offset / cos(angle + pi/2))
  } else if (is.null(data[["yintercept"]]) && is.null(data[["xintercept"]])) {
    offset_radius <- with(data, sqrt(xend^2 + yend^2))
    offset_angle <- with(data, atan2(yend, xend))
    data$yintercept <- offset_radius / sin(offset_angle)
    data$xintercept <- offset_radius / cos(offset_angle)
  }
  
  data
}

border_points_offset <- function(data, x.range, y.range) {
  # requires slope, xend, yend
  # computes x, y
  
  # compute label positions
  increasing <- sign(data$slope) == 1
  
  # (eventual) intersections with window borders
  a1 <- with(data, xend + (- yend + y.range[[1L]]) / slope)
  a2 <- with(data, xend + (- yend + y.range[[2L]]) / slope)
  b1 <- with(data, yend + (- xend + x.range[[1L]]) * slope)
  b2 <- with(data, yend + (- xend + x.range[[2L]]) * slope)
  # (bounded) intersections with window
  x1 <- pmax(x.range[[1L]], pmin(a1, a2))
  x2 <- pmin(x.range[[2L]], pmax(a1, a2))
  z1 <- pmax(y.range[[1L]], pmin(b1, b2))
  z2 <- pmin(y.range[[2L]], pmax(b1, b2))
  # account for negative slopes
  y1 <- ifelse(increasing, z1, z2)
  y2 <- ifelse(increasing, z2, z1)
  
  # farther intersection from origin
  farther2 <- x1^2 + y1^2 < x2^2 + y2^2
  transform(
    data,
    x = ifelse(farther2, x2, x1),
    y = ifelse(farther2, y2, y1)
  )
}

border_points_origin <- function(data, x.range, y.range) {
  # requires slope
  # computes x, y
  
  # compute label positions
  increasing <- sign(data$slope) == 1
  
  # (eventual) intersections with window borders
  a1 <- y.range[[1L]] / data$slope
  a2 <- y.range[[2L]] / data$slope
  b1 <- x.range[[1L]] * data$slope
  b2 <- x.range[[2L]] * data$slope
  # (bounded) intersections with window
  x1 <- pmax(x.range[[1L]], pmin(a1, a2))
  x2 <- pmin(x.range[[2L]], pmax(a1, a2))
  z1 <- pmax(y.range[[1L]], pmin(b1, b2))
  z2 <- pmin(y.range[[2L]], pmax(b1, b2))
  # account for negative slopes
  y1 <- ifelse(increasing, z1, z2)
  y2 <- ifelse(increasing, z2, z1)
  
  # farther intersection from origin
  farther2 <- x1^2 + y1^2 < x2^2 + y2^2
  transform(
    data,
    x = ifelse(farther2, x2, x1),
    y = ifelse(farther2, y2, y1)
  )
}

# TODO: Sharpen these bounds to a fixed length beyond the window.
delimit_rules <- function(data, x.range, y.range) {
  # requires x, y, radius
  # computes lower, upper
  
  # associate window boundaries to axis directions
  xtail <- ifelse(data$x > 0, x.range[[1L]], x.range[[2L]])
  xhead <- ifelse(data$x > 0, x.range[[2L]], x.range[[1L]])
  ytail <- ifelse(data$y > 0, y.range[[1L]], y.range[[2L]])
  yhead <- ifelse(data$y > 0, y.range[[2L]], y.range[[1L]])
  
  # project window corners onto axes (rule/isoline extrema)
  data$lower <- with(data, (xtail * x + ytail * y) / radius)
  data$upper <- with(data, (xhead * x + yhead * y) / radius)
  
  data
}

calibrate_rules <- function(data, by, num, loose) {
  # requires angle, radius, upper, lower, center, scale
  # accepts xend, yend
  # computes x_0, label, x_t, y_t
  # expands data to label, x_t, y_t
  
  # encode offset using fake aesthetics
  data$x_0 <- data$xend %||% 0
  data$y_0 <- data$yend %||% 0
  
  # label ranges (axis units)
  vmin <- with(data, center + scale * lower / radius)
  vmax <- with(data, center + scale * upper / radius)
  
  # element units; by default, use Wilkinson's breaks algorithm
  vseq <- if (is.null(by)) {
    lapply(seq(nrow(data)), function(i) {
      labeling::extended(vmin[[i]], vmax[[i]], num, only.loose = loose)
    })
  } else {
    if (length(by) == 1L) by <- rep(by, nrow(data))
    lapply(seq(nrow(data)), function(i) {
      vran <- if (loose)
        c(floor(vmin[[i]] / by[[i]]), ceiling(vmax[[i]] / by[[i]]))
      else
        c(ceiling(vmin[[i]] / by[[i]]), floor(vmax[[i]] / by[[i]]))
      by[[i]] * seq(vran[[1L]], vran[[2L]])
    })
  }
  data <- data[rep(seq(nrow(data)), sapply(vseq, length)), , drop = FALSE]
  data$label <- unlist(vseq)
  
  # axis positions in window units
  radius_t <- with(data, (label - center) / scale * radius)
  # NB: Use `with()` rather than `transform()` to avoid triggering NOTEs.
  data$x_t <- with(data, radius_t * cos(angle))
  data$y_t <- with(data, radius_t * sin(angle))
  
  data <- unique(data)
  data
}

offset_xy <- function(data) {
  # requires x_0, y_0 (fake aesthetics)
  # redefines x, y, xend, yend, xtick, ytick
  
  # positional variables to offset
  offset_cols <- lapply(
    c("x", "y"),
    function(xy) paste0(xy, c("", "end", "tick"))
  )
  offset_cols <- lapply(offset_cols, intersect, names(data))
  names(offset_cols) <- c("x", "y")
  
  # offset positional variables
  for (col in offset_cols$x) data[[col]] <- data[[col]] + data$x_0
  for (col in offset_cols$y) data[[col]] <- data[[col]] + data$y_0
  
  data
}

is_const <- function(x) length(unique(x)) == 1L
