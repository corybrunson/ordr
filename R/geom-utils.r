
default_arrow <- grid::arrow(
  angle = 30,
  length = unit(.02, "native"),
  ends = "last",
  type = "open"
)

# `data` must have fields 'axis_x' and 'axis_y'
calibrate_axes <- function(data, ranges, by, num) {
  
  if (is.null(by) && is.null(num)) num <- 6L
  
  # window boundaries for axis positions
  data$win_xmin <- ifelse(data$axis_x > 0, ranges$x[[1L]], ranges$x[[2L]])
  data$win_xmax <- ifelse(data$axis_x > 0, ranges$x[[2L]], ranges$x[[1L]])
  data$win_ymin <- ifelse(data$axis_y > 0, ranges$y[[1L]], ranges$y[[2L]])
  data$win_ymax <- ifelse(data$axis_y > 0, ranges$y[[2L]], ranges$y[[1L]])
  # vector lengths
  data$axis_ss <- data$axis_x ^ 2 + data$axis_y ^ 2
  # project window corners onto axis (isoline extrema), in axis units
  data$axis_min <-
    (data$win_xmin * data$axis_x + data$win_ymin * data$axis_y) / data$axis_ss
  data$axis_max <-
    (data$win_xmax * data$axis_x + data$win_ymax * data$axis_y) / data$axis_ss
  data$win_xmin <- NULL
  data$win_xmax <- NULL
  data$win_ymin <- NULL
  data$win_ymax <- NULL
  
  # label ranges
  data$label_min <- data$center + data$scale * data$axis_min
  data$label_max <- data$center + data$scale * data$axis_max
  data$axis_min <- NULL
  data$axis_max <- NULL
  
  # element units; by default, use Wilkinson's breaks algorithm
  label_vals <- if (is.null(by)) {
    lapply(seq(nrow(data)), function(i) {
      labeling::extended(data$label_min[[i]], data$label_max[[i]], num)
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
  data <- data[rep(seq(nrow(data)), sapply(label_vals, length)),
               , drop = FALSE]
  data$label <- unlist(label_vals)
  data$label_min <- NULL
  data$label_max <- NULL
  
  # axis positions
  data$axis_val <- (data$label - data$center) / data$scale
  data$x_val <- data$axis_val * data$axis_x
  data$y_val <- data$axis_val * data$axis_y
  data$axis_val <- NULL
  
  data
}

# -+- handle vertical and horizontal axes -+-
boundary_points <- function(slope, x.range, y.range) {
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
