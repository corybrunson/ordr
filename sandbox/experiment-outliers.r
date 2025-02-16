
wind_lines <- function(datum, hull) {
  
  # iterate vertical ray crossings over edges (including from last to first row)
  # include points on the boundary
  n <- nrow(hull)
  for (r in seq(n)) {
    # consecutive points along the hull
    x0 <- hull$x[(r - 1L) %% n + 1L]
    y0 <- hull$y[(r - 1L) %% n + 1L]
    x1 <- hull$x[r %% n + 1L]
    y1 <- hull$y[r %% n + 1L]
    m <- (y1 - y0) / (x1 - x0)
    
    if (x0 == x1) {
      # vertical line will not cross vertical ray
      wind <- 0
    } else {
      # may assume `x0 != x1`
      wind <- 
        # intersection of segment and vertical line is above datum
        ( m * (datum$x - x0) + y0 > datum$y ) *
        ( # edge crosses vertical line leftward
          (x0 >= datum$x & datum$x > x1)
          -
            # edge crosses vertical line rightward
            (x0 < datum$x & datum$x <= x1) )
    }
    
    segment_color <- if (wind == 0) {
      "grey"
    } else if (wind == 1) {
      "blue"
    } else if (wind == -1) {
      "red"
    } else {
      stop(paste0("wind = ", wind))
    }
    lines(x = c(x0, x1), y = c(y0, y1), col = segment_color, lwd = 2)
  }
}

t <- seq(0, 3) * pi/2
h <- data.frame(x = cos(t), y = sin(t))
s <- seq(0, 1, length.out = 25) * 2*pi
s <- s[-length(s)]
d <- data.frame(
  x = c(0, cos(s) * 2, cos(s) * .5),
  y = c(0, sin(s) * 2, sin(s) * .5)
)

# test
are_outlying(h, h)

do <- are_outlying(d, h)
plot(rbind(h, d), pch = NA)
points(h)
text(d, label = seq(nrow(d)), col = ifelse(do, "black", "grey"))
wind_lines(d[19, , drop = FALSE], h)
