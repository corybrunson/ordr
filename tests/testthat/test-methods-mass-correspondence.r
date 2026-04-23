caith_corresp <- MASS::corresp(MASS::caith)

test_that("'correspondence' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(caith_corresp)),
               ncol(get_cols(caith_corresp)))
  expect_equal(ncol(get_rows(caith_corresp)),
               length(recover_inertia(caith_corresp)))
})

test_that("'correspondence' has specified distribution of inertia", {
  expect_type(recover_conference(caith_corresp), "double")
  expect_vector(recover_conference(caith_corresp), size = 2L)
})

test_that(
  "'correspondence' augmentations are consistent with '.element' column",
  {
    expect_equal(".element" %in% names(recover_aug_rows(caith_corresp)),
                 ".element" %in% names(recover_aug_cols(caith_corresp)))
  }
)

test_that("`as_tbl_ord()` coerces 'correspondence' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(caith_corresp)))
})

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(caith_corresp)
  # inertia
  expect_identical(names(get_inertia(caith_corresp)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(caith_corresp, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(caith_corresp, elements = elts)),
      axis_names
    )
  }
})
