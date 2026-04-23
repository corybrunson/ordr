gi_eigen <- eigen(ability.cov$cov)

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(gi_eigen)
  # inertia
  expect_identical(names(get_inertia(gi_eigen)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(gi_eigen, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(gi_eigen, elements = elts)),
      axis_names
    )
  }
})

gi_eigen <- eigen_ord(ability.cov$cov)

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(gi_eigen)
  # inertia
  expect_identical(names(get_inertia(gi_eigen)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(gi_eigen, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(gi_eigen, elements = elts)),
      axis_names
    )
  }
})
