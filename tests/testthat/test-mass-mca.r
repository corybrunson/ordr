fit_mca <- MASS::mca(MASS::farms)

test_that("'mca' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_mca)), ncol(get_cols(fit_mca)))
  expect_equal(ncol(get_rows(fit_mca)), length(recover_inertia(fit_mca)))
})

test_that("'mca' has specified distribution of inertia", {
  expect_type(recover_conference(fit_mca), "double")
  expect_vector(recover_conference(fit_mca), size = 2L)
})

test_that("'mca' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_mca)),
               ".element" %in% names(recover_aug_cols(fit_mca)))
})

test_that("`as_tbl_ord()` coerces 'mca' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mca)))
})
