fit_lda1 <- MASS::lda(iris[, 1:4], iris[, 5])
fit_lda2 <- MASS::lda(Species ~ ., iris)

test_that("'lda' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_lda1)), ncol(get_cols(fit_lda1)))
  expect_equal(ncol(get_rows(fit_lda1)), length(recover_inertia(fit_lda1)))
  expect_equal(ncol(get_rows(fit_lda2)), ncol(get_cols(fit_lda2)))
  expect_equal(ncol(get_rows(fit_lda2)), length(recover_inertia(fit_lda2)))
})

test_that("'lda' has specified distribution of inertia", {
  expect_type(recover_conference(fit_lda1), "double")
  expect_vector(recover_conference(fit_lda1), size = 2L)
  expect_type(recover_conference(fit_lda2), "double")
  expect_vector(recover_conference(fit_lda2), size = 2L)
})

test_that("'lda' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_lda1)),
               ".element" %in% names(recover_aug_cols(fit_lda1)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lda2)),
               ".element" %in% names(recover_aug_cols(fit_lda2)))
})

test_that("`as_tbl_ord()` coerces 'lda' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lda1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lda2)))
})
