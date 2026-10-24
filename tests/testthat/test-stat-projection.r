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

test_that("full-dimensional projections align with full-dimensional axes", {
  pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)
  coords <- get_coord(pca)
  u <- as.matrix(get_rows(pca))[, coords]
  v <- as.matrix(get_cols(pca))[, coords]

  # window-dimensional
  p2 <- suppressWarnings(
    ggbiplot(pca) +
      geom_cols_axis(axis.type = "predictive") +
      stat_rows_projection()
  )
  l2 <- layer_data(p2, 2)
  pred2 <- sum(u[1L, 1:2] * v[1L, 1:2])
  expect_equal(
    unname(l2$xend[[1L]]),
    unname(pred2 * v[1L, 1L] / sum(v[1L, 1:2]^2))
  )

  # full-dimensional
  p1 <- suppressWarnings(
    ggbiplot(pca, ord_aes(pca)) +
      geom_cols_axis(axis.type = "predictive") +
      stat_rows_projection()
  )
  l1 <- layer_data(p1, 2)
  pred1 <- sum(u[1L, ] * v[1L, ])
  expect_equal(unname(l1$xend[[1L]]), unname(pred1 * v[1L, 1L]))
  expect_equal(unname(l1$yend[[1L]]), unname(pred1 * v[1L, 2L]))
  
  expect_false(isTRUE(all.equal(l1$xend[[1L]], l2$xend[[1L]])))
})
