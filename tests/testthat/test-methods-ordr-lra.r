arrests_lra <- lra(subset(USArrests, select = -UrbanPop))

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(arrests_lra)
  # inertia
  expect_identical(names(get_inertia(arrests_lra)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(arrests_lra, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(arrests_lra, elements = elts)),
      axis_names
    )
  }
})
