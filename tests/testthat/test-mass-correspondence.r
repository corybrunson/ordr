fit_correspondence <- MASS::corresp(MASS::caith)

test_that("'correspondence' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_correspondence)),
               ncol(get_cols(fit_correspondence)))
  expect_equal(ncol(get_rows(fit_correspondence)),
               length(recover_inertia(fit_correspondence)))
})

test_that("'correspondence' has specified distribution of inertia", {
  expect_type(recover_conference(fit_correspondence), "double")
  expect_vector(recover_conference(fit_correspondence), size = 2L)
})

test_that(
  "'correspondence' augmentations are consistent with '.element' column",
  {
    expect_equal(".element" %in% names(recover_aug_rows(fit_correspondence)),
                 ".element" %in% names(recover_aug_cols(fit_correspondence)))
  }
)

test_that("`as_tbl_ord()` coerces 'correspondence' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_correspondence)))
})
