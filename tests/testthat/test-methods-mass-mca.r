ucb_admissions <- as.data.frame(UCBAdmissions)
ucb_admissions <-
  ucb_admissions[rep(seq(nrow(ucb_admissions)), ucb_admissions$Freq), -4L]
admissions_mca <- MASS::mca(ucb_admissions)

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(admissions_mca)
  # inertia
  expect_identical(names(get_inertia(admissions_mca)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(admissions_mca, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(admissions_mca, elements = elts)),
      axis_names
    )
  }
})
