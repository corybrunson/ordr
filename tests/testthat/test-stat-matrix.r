test_that("helper functions handle strings and ggprotos", {
  # identity string
  expect_equal(rows_stat("identity"), "rows")
  expect_equal(cols_stat("identity"), "cols")
  # other string
  expect_equal(rows_stat("suffix"), "rows_suffix")
  expect_equal(cols_stat("suffix"), "cols_suffix")
  # parent ggproto
  expect_error(rows_stat(Stat))
  expect_error(cols_stat(Stat))
  # identity ggproto
  expect_equal(rows_stat(StatIdentity), "rows")
  expect_equal(cols_stat(StatIdentity), "cols")
  # other ggproto
  expect_equal(rows_stat(StatCenter), "rows_center")
  expect_equal(cols_stat(StatDepth), "cols_depth")
})

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
