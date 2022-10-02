fit_kmeans <- kmeans(scale(mtcars), centers = 3)

test_that("'kmeans' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_kmeans)), ncol(get_cols(fit_kmeans)))
})

test_that("'kmeans' inertia cannot be accessed", {
  expect_equal(recover_inertia(fit_kmeans), NA_real_)
})

test_that("'kmeans' does not confer inertia", {
  expect_null(recover_conference(fit_kmeans))
})

test_that("'kmeans' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_kmeans)),
               ".element" %in% names(recover_aug_cols(fit_kmeans)))
})

test_that("`as_tbl_ord()` coerces 'kmeans' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_kmeans)))
})
