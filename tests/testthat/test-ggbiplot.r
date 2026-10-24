pca <- confer_inertia(as_tbl_ord(prcomp(USPersonalExpenditure)), "symmetric")

test_that("`ggplot()` successfully passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "rows"))),
               nrow(USPersonalExpenditure))
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "cols"))),
               ncol(USPersonalExpenditure))
})

test_that("`ggbiplot()` handles coordinate aesthetics", {
  expect_true(all(c("x", "y") %in% names(ggbiplot(pca)$mapping)))
  expect_true(all(c("x", "y") %in% names(ggbiplot(pca, aes(y = 3))$mapping)))
  expect_no_error(print(ggbiplot(pca) + geom_rows_point()))
  expect_error(print(ggbiplot(pca, aes(x = 2, y = 0)) + geom_rows_point()),
               regexp = "select")
  expect_no_error(
    print(ggbiplot(pca, aes(x = 2, y = 0 + 0)) + geom_rows_point())
  )
})

test_that("`ggbiplot()` calibrates predictive axes", {
  pca <- ordinate(iris, cols = 1:4, prcomp, scale = TRUE)
  coords <- get_coord(pca)
  v <- as.matrix(get_cols(pca))[, coords]

  # plot window rescalings agree
  ss1 <- v[, 1]^2 + v[, 2]^2
  p1 <- suppressWarnings(
    ggbiplot(pca) + geom_cols_axis(axis.type = "predictive")
  )
  l1 <- layer_data(p1)
  q1 <- suppressWarnings(
    ggbiplot(pca) + geom_cols_axis(axis.type = "interpolative")
  )
  k1 <- layer_data(q1)
  expect_equal(l1$axis.ss, unname(ss1))
  expect_equal(l1$radius, unname(sqrt(ss1) / ss1))
  expect_equal(unname(k1$radius / ss1), l1$radius)
  
  # full-dimensional rescalings agree
  ss2 <- rowSums(v^2)
  p2 <- suppressWarnings(
    ggbiplot(pca, ord_aes(pca)) + geom_cols_axis(axis.type = "predictive")
  )
  l2 <- layer_data(p2)
  q2 <- suppressWarnings(
    ggbiplot(pca, ord_aes(pca)) + geom_cols_axis(axis.type = "interpolative")
  )
  k2 <- layer_data(q2)
  expect_equal(l2$axis.ss, unname(ss2))
  expect_equal(l2$radius, unname(sqrt(v[, 1]^2 + v[, 2]^2) / ss2))
  expect_equal(unname(k2$radius / ss2), l2$radius)
})
