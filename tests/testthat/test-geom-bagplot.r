test_that("`geom_bagplot()` accepts contrived data with required aesthetics", {
  t <- seq(0, 2) * 2/3*pi
  d <- data.frame(
    x = c(0, cos(t), 2*cos(t), 3*cos(t)),
    y = c(0, sin(t), 2*sin(t), 3*sin(t)),
    component = c(
      "median",
      rep("bag", 3L),
      rep("fence", 3L),
      rep("outliers", 3L)
    )
  )
  expect_no_error(
    p <- ggplot(d, aes(x, y, component = component)) + 
      geom_bagplot(stat = "identity")
  )
})
