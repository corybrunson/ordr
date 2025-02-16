d <- data.frame(x = 3, y = 4)
p <- ggplot(d, aes(x, y))

test_that("default settings yield multiple isolines", {
  p1 <- p + geom_isoline()
  # only one row
  l1 <- layer_data(p1)
  expect_equal(nrow(l1), 1L)
  # several lines
  g1 <- layer_grob(p1)
  expect_true(length(g1[[1L]]$children[[1L]]) > 2)
})

# TODO: Test that endpoints lie outside plotting window.
