
default_arrow <- grid::arrow(
  angle = 30,
  length = unit(.02, "native"),
  ends = "last",
  type = "open"
)

# `data` must have fields 'axis_x' and 'axis_y'
calibrate_rules_old <- function(data, by, num) {
  
  if (is.null(by) && is.null(num)) num <- 6L
  
  # FIXME: Experimenting to resolve rule bound discrepancy.
  # vector inertias
  data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
  # label ranges
  data$label_min <- data$center + data$scale * data$lower / data$axis_ss
  data$label_max <- data$center + data$scale * data$upper / data$axis_ss
  
  # element units; by default, use Wilkinson's breaks algorithm
  label_vals <- if (is.null(by)) {
    lapply(seq(nrow(data)), function(i) {
      labeling::extended(
        data$label_min[[i]], data$label_max[[i]], num,
        only.loose = TRUE
      )
    })
  } else {
    if (length(by) == 1L) by <- rep(by, nrow(data))
    lapply(seq(nrow(data)), function(i) {
      by[[i]] * seq(
        floor(data$label_min[[i]] / by[[i]]),
        ceiling(data$label_max[[i]] / by[[i]])
      )
    })
  }
  data <- data[rep(seq(nrow(data)), sapply(label_vals, length)), , drop = FALSE]
  data$label <- unlist(label_vals)
  data$label_min <- NULL
  data$label_max <- NULL
  
  # axis positions in window units
  data$axis_val <- (data$label - data$center) / data$scale
  data$x_val <- data$axis_val * data$axis_x
  data$y_val <- data$axis_val * data$axis_y
  data$axis_val <- NULL
  
  data
  
}

# `data` must have fields 'axis_x' and 'axis_y'
calibrate_axes <- function(data, ranges, by, num) {
  
  # window boundaries for axis positions, relative to axis direction
  data$win_xtail <- ifelse(data$axis_x > 0, ranges$x[[1L]], ranges$x[[2L]])
  data$win_xhead <- ifelse(data$axis_x > 0, ranges$x[[2L]], ranges$x[[1L]])
  data$win_ytail <- ifelse(data$axis_y > 0, ranges$y[[1L]], ranges$y[[2L]])
  data$win_yhead <- ifelse(data$axis_y > 0, ranges$y[[2L]], ranges$y[[1L]])
  # FIXME: Experimenting to resolve rule bound discrepancy.
  # project window corners onto axis (isoline extrema), in axis units
  data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
  data$lower <-
    (data$win_xtail * data$axis_x + data$win_ytail * data$axis_y) / data$axis_ss
  data$upper <-
    (data$win_xhead * data$axis_x + data$win_yhead * data$axis_y) / data$axis_ss
  # # project window corners onto axis (isoline extrema) in window units
  # data$lower <- data$win_xtail * data$axis_x + data$win_ytail * data$axis_y
  # data$upper <- data$win_xhead * data$axis_x + data$win_yhead * data$axis_y
  data$win_xtail <- NULL
  data$win_xhead <- NULL
  data$win_ytail <- NULL
  data$win_yhead <- NULL
  
  calibrate_rules_old(data, by, num)
}

limit_values <- function(x, y, x.range, y.range) {
  
  # associate window boundaries to axis directions
  xtail <- ifelse(x > 0, x.range[[1L]], x.range[[2L]])
  xhead <- ifelse(x > 0, x.range[[2L]], x.range[[1L]])
  ytail <- ifelse(y > 0, y.range[[1L]], y.range[[2L]])
  yhead <- ifelse(y > 0, y.range[[2L]], y.range[[1L]])
  
  # project window corners onto axes (isoline extrema)
  mag <- sqrt(x^2 + y^2)
  lower <- (xtail * x + ytail * y) / mag
  upper <- (xhead * x + yhead * y) / mag
  
  data.frame(lower = lower, upper = upper)
}

calibrate_rules <- function(data, by, num, loose) {
  req_names <- 
    c("axis_x", "axis_y", "axis_ss", "upper", "lower", "center", "scale")
  req_missing <- which(! req_names %in% names(data))
  if (any(req_missing)) {
    stop(
      "Columns are missing from data: ",
      paste0(req_names[req_missing], collapse = ", ")
    )
  }
  if (is.null(by) && is.null(num)) num <- 6L
  
  # label ranges (axis units)
  vmin <- with(data, center + scale * lower / sqrt(axis_ss))
  vmax <- with(data, center + scale * upper / sqrt(axis_ss))
  
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
      by[[i]] * seq(vran[[1]], vran[[2]])
    })
  }
  data <- data[rep(seq(nrow(data)), sapply(vseq, length)), , drop = FALSE]
  data$label <- unlist(vseq)
  
  # axis positions in window units
  axis_val <- (data$label - data$center) / data$scale
  data$x_val <- axis_val * data$axis_x
  data$y_val <- axis_val * data$axis_y
  
  data
}

# -+- handle vertical and horizontal axes -+-
border_points <- function(slope, x.range, y.range) {
  res <- data.frame(slope = slope)
  
  # compute label positions
  res$increasing <- sign(res$slope) == 1L
  
  # (eventual) intersections with window borders
  res$a1 <- y.range[[1L]] / res$slope
  res$a2 <- y.range[[2L]] / res$slope
  res$b1 <- x.range[[1L]] * res$slope
  res$b2 <- x.range[[2L]] * res$slope
  # (bounded) intersections with window
  res$x1 <- pmax(x.range[[1L]], pmin(res$a1, res$a2))
  res$x2 <- pmin(x.range[[2L]], pmax(res$a1, res$a2))
  res$z1 <- pmax(y.range[[1L]], pmin(res$b1, res$b2))
  res$z2 <- pmin(y.range[[2L]], pmax(res$b1, res$b2))
  # account for negative slopes
  res$y1 <- ifelse(res$increasing, res$z1, res$z2)
  res$y2 <- ifelse(res$increasing, res$z2, res$z1)
  # distances from origin
  res$rsq1 <- res$x1 ^ 2 + res$y1 ^ 2
  res$rsq2 <- res$x2 ^ 2 + res$y2 ^ 2
  # farther intersection from origin
  res$x <- ifelse(res$rsq1 < res$rsq2, res$x2, res$x1)
  res$y <- ifelse(res$rsq1 < res$rsq2, res$y2, res$y1)
  
  res[, c("x", "y")]
}
