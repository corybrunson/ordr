fit_lda_ord1 <- lda_ord(iris[, 1:4], iris[, 5])
fit_lda_ord2 <- lda_ord(Species ~ ., iris)
fit_lda_ord3 <- lda_ord(iris[, 1:4], iris[, 5], ret.x = TRUE)

test_that("'lda' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_lda_ord1)), ncol(get_cols(fit_lda_ord1)))
  expect_equal(ncol(get_rows(fit_lda_ord1)),
               length(recover_inertia(fit_lda_ord1)))
  expect_equal(ncol(get_rows(fit_lda_ord2)), ncol(get_cols(fit_lda_ord2)))
  expect_equal(ncol(get_rows(fit_lda_ord2)),
               length(recover_inertia(fit_lda_ord2)))
  expect_equal(ncol(get_rows(fit_lda_ord3)), ncol(get_cols(fit_lda_ord3)))
  expect_equal(ncol(get_rows(fit_lda_ord3)),
               length(recover_inertia(fit_lda_ord3)))
})

test_that("'lda' has specified distribution of inertia", {
  expect_type(recover_conference(fit_lda_ord1), "double")
  expect_vector(recover_conference(fit_lda_ord1), size = 2L)
  expect_type(recover_conference(fit_lda_ord2), "double")
  expect_vector(recover_conference(fit_lda_ord2), size = 2L)
  expect_type(recover_conference(fit_lda_ord3), "double")
  expect_vector(recover_conference(fit_lda_ord3), size = 2L)
})

test_that("'lda' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_lda_ord1)),
               ".element" %in% names(recover_aug_cols(fit_lda_ord1)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lda_ord2)),
               ".element" %in% names(recover_aug_cols(fit_lda_ord2)))
  expect_equal(".element" %in% names(recover_aug_rows(fit_lda_ord3)),
               ".element" %in% names(recover_aug_cols(fit_lda_ord3)))
})

test_that("`as_tbl_ord()` coerces 'lda' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lda_ord1)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lda_ord2)))
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lda_ord3)))
})
