pca <- confer_inertia(as_tbl_ord(prcomp(USPersonalExpenditure)), "symmetric")

test_that("`ggplot()` successfully passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "rows"))),
               nrow(USPersonalExpenditure))
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "cols"))),
               ncol(USPersonalExpenditure))
})

test_that("`ggbiplot()` handles coordinate aesthetics", {
  expect_in(c("x", "y"), names(ggbiplot(pca)$mapping))
  
  expect_in("x", names(ggbiplot(pca, aes(x = 3))$mapping))
  expect_false("y" %in% names(ggbiplot(pca, aes(x = 3))$mapping))
  expect_in("y", names(ggbiplot(pca, aes(y = 3))$mapping))
  expect_false("x" %in% names(ggbiplot(pca, aes(y = 3))$mapping))
  
  expect_no_error(print(ggbiplot(pca) + geom_rows_point()))
  expect_error(print(ggbiplot(pca, aes(x = 2, y = 0)) + geom_rows_point()),
               regexp = "select")
  expect_no_error(print(ggbiplot(pca, aes(x = 2, y = 0 + 0)) + 
                          geom_rows_point()))
})
