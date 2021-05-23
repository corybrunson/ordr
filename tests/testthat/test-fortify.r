context("fortification")

# arbitrary ordination object
dat <- USPersonalExpenditure
pca <- as_tbl_ord(prcomp(dat))
pca <- confer_inertia(pca, "symmetric")

test_that("`fortify()` returns tibbles of correct size", {
  expect_equal(nrow(fortify(pca, .matrix = "rows")), nrow(dat))
  expect_equal(nrow(fortify(pca, .matrix = "cols")), ncol(dat))
})

test_that("`ggplot()` successfully passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "rows"))), nrow(dat))
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "cols"))), ncol(dat))
})

dims <- dim(dat)
test_that("`ggbiplot()` passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "rows"))), dims[1])
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "cols"))), dims[2])
})
