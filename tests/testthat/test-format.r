ord <- prcomp(iris[, 1:4])

# adapted from tibble tests
test_that("`print()` returns object invisibly", {
  expect_output(ret <- withVisible(print(as_tbl_ord(ord))))
  expect_false(ret$visible)
  expect_identical(ret$value, as_tbl_ord(ord))
})
