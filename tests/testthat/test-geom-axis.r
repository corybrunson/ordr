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
polar_data <- data.frame(
  t = t,
  r = 1
)
polar_plot <- 
  ggplot(polar_data, aes(angle = t, radius = r)) +
  coord_equal() +
  geom_axis()
polar_layer <- layer_data(polar_plot)

test_that("`geom_axis()` ensures `angle,radius` and hides `x,y`", {
  expect_in(c("angle", "radius"), names(cartesian_layer))
  expect_true(all(! c("x", "y") %in% names(cartesian_layer)))
  
  expect_in(c("angle", "radius"), names(polar_layer))
  expect_true(all(! c("x", "y") %in% names(polar_layer)))
  
  expect_equivalent(
    cartesian_layer[, c("angle", "radius")],
    polar_layer[, c("angle", "radius")]
  )
})
