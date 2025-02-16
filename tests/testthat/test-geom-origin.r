test_that("origin and unit circle do not affect plotting window", {
  
  # point cloud outside the unit circle
  set.seed(695839L)
  d <- data.frame(
    a = runif(3, min = 2, max = 3),
    b = runif(3, min = -4, max = -3)
  )
  # template plot
  p <- ggplot(d, aes(a, b)) + geom_point()
  
  # origin only
  b1 <- ggplot_build(p + geom_origin())
  x1 <- b1$layout$panel_scales_x[[1]]$range$range
  y1 <- b1$layout$panel_scales_y[[1]]$range$range
  expect_true(0 < min(x1))
  expect_true(0 > max(y1))
  
  # unit circle only
  b2 <- ggplot_build(p + geom_unit_circle())
  x2 <- b2$layout$panel_scales_x[[1]]$range$range
  y2 <- b2$layout$panel_scales_y[[1]]$range$range
  expect_true(1 < min(x2))
  expect_true(-1 > max(y2))
  
})
