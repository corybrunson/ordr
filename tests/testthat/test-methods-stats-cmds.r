euro_cmds <- cmdscale_ord(eurodist, k = 3)

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(euro_cmds)
  # inertia
  expect_identical(names(get_inertia(euro_cmds)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(euro_cmds, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(euro_cmds, elements = elts)),
      axis_names
    )
  }
})
