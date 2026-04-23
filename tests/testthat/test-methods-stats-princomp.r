iris_pca <- princomp(iris[, -5])

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(iris_pca)
  # inertia
  expect_identical(names(get_inertia(iris_pca)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(iris_pca, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(iris_pca, elements = elts)),
      axis_names
    )
  }
})
