iris_pca <- as_tbl_ord(prcomp(iris[, 1:4]))

test_that("negation is consistent via different input formats", {
  res <- get_negation(negate_ord(iris_pca, c(2, 3)))
  expect_equal(res, get_negation(negate_ord(iris_pca, c("PC2", "PC3"))))
  expect_equal(res, get_negation(negate_ord(iris_pca, c(1, -1, -1, 1))))
})

test_that("negated ordinations preserve coordinate names", {
  expect_equal(
    get_coord(iris_pca),
    get_coord(negate_ord(iris_pca, c(1, 3)))
  )
})
