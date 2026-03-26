test_that("projection segments align with main & referent data", {
  
  # toy data
  t <- seq(3)/3 * 2*pi
  d1 <- data.frame(x1 = cos(t), x2 = sin(t))
  d2 <- data.frame(x1 = cos(1), x2 = sin(1))
  # plot layers
  p <- ggplot(d1, aes(x1, x2)) + 
    geom_segment(aes(xend = 0, yend = 0)) + 
    stat_projection(referent = d2)
  layer_data(p, 1) %>%
    subset(select = c(x, y)) %>%
    as.matrix() -> m1
  layer_data(p, 2) %>%
    transform(dx = x - xend, dy = y - yend) %>%
    subset(select = c(dx, dy)) %>%
    as.matrix() -> m2
  layer_data(p, 2) %>%
    subset(select = c(xend, yend)) %>%
    lapply(diff) %>% as.data.frame() %>%
    as.matrix() -> m3
  
  # projection segments begin from main data
  expect_identical(unname(m1), unname(as.matrix(d1)))
  
  # projection segments are orthogonal to referent data
  expect_equal(max(abs(as.matrix(d2) %*% t(m2))), 0, tolerance = 1e-15)
  
  # projection segments lie on a line parallel to the referent data
  scalars <- sweep(m3, 2, unlist(d2), "/") %>% apply(2, diff)
  expect_equal(scalars[[1]], scalars[[2]])
  
})
