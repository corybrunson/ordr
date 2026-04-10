test_that("matrix factor stats don't throw errors", {
  
  pca <- as_tbl_ord(prcomp(swiss))
  p <- ggbiplot(pca)
  
  # matrix factor stats
  expect_no_error(p + stat_rows(geom = "point"))
  expect_no_error(p + stat_cols(geom = "vector"))
  
  # with additional parameters
  expect_no_error(p + stat_rows(geom = "point", elements = "scores"))
  expect_no_error(p + stat_cols(geom = "vector", subset = 1:2))
  
})
