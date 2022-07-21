context("accessors")

spend_pca <- prcomp(USPersonalExpenditure, center = FALSE)
spend_pca_ord <- as_tbl_ord(spend_pca)

test_that("row and column vectors are recovered", {
  expect_identical(spend_pca$x, get_rows(spend_pca_ord))
  expect_identical(spend_pca$rotation, get_cols(spend_pca_ord))
})
