t <- asin(sqrt(seq(0, 3)) / 2)
t <- c(t, t + pi/2)
cartesian_data <- data.frame(
  x = cos(t),
  y = sin(t)
)
cartesian_plot <- 
  ggplot(cartesian_data, aes(x, y)) +
  coord_equal() +
  geom_axis()
cartesian_layer <- layer_data(cartesian_plot)

test_that("`geom_axis()` converts `x,y` to `angle,radius`", {
  expect_in(c("angle", "radius"), names(cartesian_layer))
  expect_true(all(! c("x", "y") %in% names(cartesian_layer)))
  
  expect_equivalent(
    cartesian_layer[, c("angle", "radius")],
    data.frame(angle = t, radius = 1)
  )
})
