context("accessors")

spend_pca <- prcomp(USPersonalExpenditure, center = FALSE)
spend_pca_ord <- as_tbl_ord(spend_pca)
haireye_ca <- ca::ca(HairEyeColor[, , "Female"])
haireye_ca_ord <- as_tbl_ord(haireye_ca)

test_that("row and column vectors are recovered", {
  expect_identical(spend_pca$x, get_rows(spend_pca_ord))
  expect_identical(spend_pca$rotation, get_cols(spend_pca_ord))
  expect_identical(haireye_ca$rowcoord, get_rows(haireye_ca_ord))
  expect_identical(haireye_ca$colcoord, get_cols(haireye_ca_ord))
})

test_that("inertia is recovered", {
  expect_true(all(get_inertia(haireye_ca_ord) / (haireye_ca$sv^2) == 1))
})
