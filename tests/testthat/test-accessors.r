context("accessors")

spend_pca <- prcomp(USPersonalExpenditure, center = FALSE)
spend_pca_ord <- as_tbl_ord(spend_pca)
haireye_ca <- ca::ca(HairEyeColor[, , "Female"])
haireye_ca_ord <- as_tbl_ord(haireye_ca)

test_that("row and column vectors are recovered", {
  expect_identical(spend_pca$x, recover_rows(spend_pca_ord))
  expect_identical(spend_pca$rotation, recover_cols(spend_pca_ord))
  expect_identical(haireye_ca$rowcoord, recover_rows(haireye_ca_ord))
  expect_identical(haireye_ca$colcoord, recover_cols(haireye_ca_ord))
})

test_that("inertia is recovered", {
  expect_true(all(recover_inertia(spend_pca_ord) / (spend_pca$sdev^2) ==
                    dim(spend_pca_ord) - 1))
  expect_true(all(recover_inertia(haireye_ca_ord) / (haireye_ca$sv^2) == 1))
})

test_that("coordinates are recovered", {
  expect_true(length(recover_coord(spend_pca_ord)) == dim(spend_pca_ord))
  expect_true(length(recover_coord(haireye_ca_ord)) == dim(haireye_ca_ord))
})
