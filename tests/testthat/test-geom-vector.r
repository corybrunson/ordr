r <- seq(5); t <- seq(5) * 2*pi/5
d <- data.frame(a = r*cos(t), b = r*sin(t), alpha = LETTERS[r])
p <- ggplot(d, aes(a, b, label = alpha))

test_that("`geom_vector()` correctly handles secondary aesthetics", {
  g <- layer_grob(
    p + geom_vector(label.colour = "blue", label.alpha = .75)
  )
  # label color and opacity
  expect_setequal(g[[1L]]$children[[2L]]$gp$col, "#0000FFBF")
})

test_that("`geom_vector()` respects element inclusion parameters", {
  # no labels
  g1 <- layer_grob(p + geom_vector(vector_labels = FALSE))
  expect_equal(length(g1[[1L]]$children), 1L)
  # labels
  g2 <- layer_grob(p + geom_vector(vector_labels = TRUE))
  expect_equal(length(g2[[1L]]$children), 2L)
})
