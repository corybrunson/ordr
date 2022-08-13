ed <- as_tbl_ord(eigen_ord(cbind(c(1,-1), c(-1,1))))
fa <- as_tbl_ord(factanal(swiss, factors = 3L, scores = "regression"))

ed_aug <- augment_ord(ed)
fa_aug <- augment_ord(fa)

test_that("augmentation preserves 'tbl_ord' class", {
  expect_s3_class(ed_aug, "tbl_ord")
  expect_s3_class(fa_aug, "tbl_ord")
})

test_that("augmentation preserves factor dimensions", {
  expect_equal(nrow(get_rows(ed_aug)), nrow(get_rows(ed)))
  expect_equal(nrow(get_cols(ed_aug)), nrow(get_cols(ed)))
  expect_equal(nrow(get_rows(fa_aug)), nrow(get_rows(fa)))
  expect_equal(nrow(get_cols(fa_aug)), nrow(get_cols(fa)))
})

test_that("glancing returns a single row", {
  expect_equal(nrow(glance(ed)), 1L)
  expect_equal(nrow(glance(fa)), 1L)
})

test_that("tidying includes 'inertia' and 'name' fields", {
  expect_true(all(c("inertia", "name") %in% names(tidy(ed))))
  expect_true(all(c("inertia", "name") %in% names(tidy(fa))))
})
