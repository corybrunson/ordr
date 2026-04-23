iris_lda <- MASS::lda(iris[, 1:4], iris[, 5])

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(iris_lda)
  # inertia
  expect_identical(names(get_inertia(iris_lda)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(iris_lda, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(iris_lda, elements = elts)),
      axis_names
    )
  }
})

iris_lda <- lda_ord(iris[, 1:4], iris[, 5])

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(iris_lda)
  # inertia
  expect_identical(names(get_inertia(iris_lda)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(iris_lda, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(iris_lda, elements = elts)),
      axis_names
    )
  }
})
