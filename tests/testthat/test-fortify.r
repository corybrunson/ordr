library(ordr)
context("fortification")

# arbitrary ordination object
pca <- as_tbl_ord(prcomp(USArrests))
pca <- confer_inertia(pca, "symmetric")

test_that("`fortify()` returns tibbles of correct size", {
  expect_equal(nrow(fortify(pca, .matrix = "u")), nrow(USArrests))
  expect_equal(nrow(fortify(pca, .matrix = "v")), ncol(USArrests))
  expect_equal(nrow(fortify(pca, .matrix = "coord")), dim(pca))
})

test_that("`ggplot()` successfully passes `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "u"))), nrow(USArrests))
  expect_equal(nrow(layer_data(ggplot(pca, .matrix = "v"))), ncol(USArrests))
})

sumdim <- sum(dim(USArrests))
test_that("`ggbiplot()` does _not_ pass `.matrix` to `fortify()`", {
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "u"))), sumdim)
  expect_equal(nrow(layer_data(ggbiplot(pca, .matrix = "v"))), sumdim)
})
