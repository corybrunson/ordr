context("k-means clustering, class 'kmeans'")

fit_kmeans <- kmeans(scale(mtcars), centers = 3)
test_that("`as_tbl_ord()` coerces 'kmeans' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_kmeans)))
})
