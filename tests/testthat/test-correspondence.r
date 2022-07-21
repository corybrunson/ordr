context("correspondence analysis, class 'correspondence'")

fit_correspondence <- MASS::corresp(~ Age + Eth, data = MASS::quine)

test_that("`as_tbl_ord()` coerces 'correspondence' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_correspondence)))
})

test_that("'corresp.' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(augmentation_rows(fit_correspondence)),
               ".element" %in% names(augmentation_cols(fit_correspondence)))
})
