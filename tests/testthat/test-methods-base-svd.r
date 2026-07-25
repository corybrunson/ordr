iris_svd <- svd_ord(scale(iris[, 1:4]))

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(iris_svd)
  # inertia
  expect_identical(names(get_inertia(iris_svd)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(iris_svd, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(iris_svd, elements = elts)),
      axis_names
    )
  }
})
