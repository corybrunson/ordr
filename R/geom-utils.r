
default_arrow <- grid::arrow(
  angle = 30,
  length = unit(.02, "native"),
  ends = "last",
  type = "open"
)

# introduce `x` & `y` if passed only `angle` & `radius` and vice-versa
# (read `angle` as radians)
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

# -+- handle vertical and horizontal axes -+-
# TODO: Take offset into account.
border_points_offset <- function(data, x.range, y.range) {
  # x, y, angle, radius, yintercept, xintercept, xend, yend, dodge_angle
  
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

delimit_rules <- function(data, x.range, y.range) {
  
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
  # requires columns x, y, radius, angle, upper, lower, center, scale
  
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
