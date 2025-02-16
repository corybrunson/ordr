d <- data.frame(
  abscissa = 3, ordinate = 4,
  zero = -1, one = 10,
  start = -0.6, end = 0.2,
  name = "Pythagorean triple"
)
d$length <- sqrt( sum( d$abscissa^2 + d$ordinate^2 ) )
p <- ggplot(d, aes(
  abscissa, ordinate,
  center = zero, scale = one,
  lower = start, upper = end,
  label = name
)) +
  coord_equal()

test_that("`geom_rule()` correctly handles required aesthetics", {
  p0 <- p + geom_rule()
  l0 <- layer_data(p0)
  g0 <- layer_grob(p0)
  b0 <- ggplot_build(p0)
  
  # single axis, single row
  expect_equal(nrow(p0$data), 1L)
  expect_equal(nrow(l0), 1L)
  expect_equal(nrow(b0$data[[1L]]), 1L)
  
  # segment limits are correctly positioned
  ( as.matrix(unlist(d[c("start", "end")])) %*% 
      unlist(d[c("abscissa", "ordinate")]) / d$length ) |> 
    as.vector() ->
    endpts
  expect_equal(unname(unlist(l0[c("xmin", "xmax", "ymin", "ymax")])), endpts)
  
  # segment limits are reasonably calibrated
  tcklbs <- as.numeric(g0[[1L]]$children[[4L]]$label)
  tckran <- c(d$start, d$end) / d$length * d$one + d$zero
  if (tckran[[1L]] > tckran[[2L]]) {
    tcklbs <- rev(tcklbs)
    tckran <- rev(tckran)
  }
  expect_true(all(tcklbs >= tckran[1L]))
  expect_true(all(tcklbs <= tckran[2L]))
})

test_that("`geom_rule()` respects element inclusion parameters", {
  # no labels
  g1 <- layer_grob(p + geom_rule(axis_labels = FALSE))
  expect_equal(length(g1[[1L]]$children), 3L)
  # no ticks
  g2 <- layer_grob(p + geom_rule(axis_ticks = FALSE))
  expect_equal(length(g2[[1L]]$children), 3L)
  # no text
  g3 <- layer_grob(p + geom_rule(axis_text = FALSE))
  expect_equal(length(g3[[1L]]$children), 3L)
  # no labels, no ticks
  g4 <- layer_grob(p + geom_rule(axis_labels = FALSE, axis_ticks = FALSE))
  expect_equal(length(g4[[1L]]$children), 2L)
  # no labels, no text
  g5 <- layer_grob(p + geom_rule(axis_labels = FALSE, axis_text = FALSE))
  expect_equal(length(g5[[1L]]$children), 2L)
  # no ticks, no text
  g6 <- layer_grob(p + geom_rule(axis_ticks = FALSE, axis_text = FALSE))
  expect_equal(length(g6[[1L]]$children), 2L)
  # no labels, ticks, or text
  g7 <- layer_grob(p + geom_rule(axis_labels = FALSE, axis_ticks = FALSE, axis_text = FALSE))
  expect_equal(length(g7[[1L]]$children), 1L)
})

test_that("`geom_rule()` correctly handles secondary aesthetics", {
  g8 <- layer_grob(
    p + geom_rule(
      axis.color = "red",
      label.angle = 30,
      tick.linewidth = 1,
      text.fontface = "italic"
    )
  )
  # axis color
  expect_equal(g8[[1L]]$children[[1L]]$gp$col, "#FF0000FF")
  # label angle
  axis_rot <- atan2(d$ordinate, d$abscissa) * 180 / pi
  expect_equal(g8[[1L]]$children[[2L]]$rot, axis_rot + 30)
  # tick linewidth
  axis_lwd <- g8[[1L]]$children[[1L]]$gp$lwd
  expect_setequal(g8[[1L]]$children[[3L]]$gp$lwd, 4 * axis_lwd)
  # text font
  expect_setequal(unname(g8[[1L]]$children[[4L]]$gp$font), 3L)
  expect_setequal(names(g8[[1L]]$children[[4L]]$gp$font), "italic")
})
