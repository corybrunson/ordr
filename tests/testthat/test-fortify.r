library(ordr)
context("fortification")

# arbitrary ordination object
dat <- USPersonalExpenditure
pca <- as_tbl_ord(prcomp(dat))
pca <- confer_inertia(pca, "symmetric")

test_that("`fortify()` returns tibbles of correct size", {
  expect_equal(nrow(fortify(pca, .matrix = "u")), nrow(dat))
  expect_equal(nrow(fortify(pca, .matrix = "v")), ncol(dat))
  expect_equal(nrow(fortify(pca, .matrix = "coord")), dim(pca))
})

test_that("`ggplot()` successfully passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "u"))), nrow(dat))
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "v"))), ncol(dat))
})

sumdim <- sum(dim(dat))
test_that("`ggbiplot()` does _not_ pass `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "u"))), sumdim)
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "v"))), sumdim)
})
