swiss_factanal <-
  factanal(~ ., factors = 2L, data = swiss, scores = "regression")

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(swiss_factanal)
  # inertia
  expect_identical(names(get_inertia(swiss_factanal)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(swiss_factanal, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(swiss_factanal, elements = elts)),
      axis_names
    )
  }
})
