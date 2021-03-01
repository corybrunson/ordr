context("augmentation")

spend_pca <- as_tbl_ord(prcomp(USPersonalExpenditure, center = FALSE))
haireye_ca <- as_tbl_ord(ca::ca(HairEyeColor[, , "Male"]))

test_that("augmentation preserves 'tbl_ord' class", {
  expect_true(valid_tbl_ord(augment_ord(spend_pca)))
  expect_true(valid_tbl_ord(augment_ord(haireye_ca)))
})

test_that("tidying includes '.inertia' and '.name' fields", {
  expect_true(all(c(".inertia", ".name") %in% names(tidy(spend_pca))))
  expect_true(all(c(".inertia", ".name") %in% names(tidy(haireye_ca))))
})
