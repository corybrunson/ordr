context("multiple correspondence analysis, class 'mca'")

fit_mca <- MASS::mca(MASS::farms)

test_that("`as_tbl_ord()` coerces 'mca' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_mca)))
})

test_that("'mca' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_mca)),
               ".element" %in% names(augmentation_cols(fit_mca)))
})
