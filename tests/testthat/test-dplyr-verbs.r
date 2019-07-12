library(ordr)
context("dplyr verbs for ordination factors")

# arbitrary ordination object
pca <- augment(as_tbl_ord(prcomp(iris[, -5])))

test_that("`pull_*()` returns a vector", {
  expect_equal(pull_v(pca, .name), names(iris)[-5])
})

test_that("`bind_cols_*()` appends a column only of the correct length", {
  expect_equal(
    ncol(tidy(bind_cols_u(pca, iris[, 5, drop = FALSE]), .matrix = "u")),
    4 + 1 + 1
  )
  expect_equal(
    ncol(tidy(bind_cols_u(pca, species = iris[[5]]), .matrix = "u")),
    4 + 1 + 1
  )
  expect_error(bind_cols_u(pca, letter = letters), "length")
})
