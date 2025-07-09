# toy example with all positive values of EV1
cbind(c(1, 1, 1), c(1, 2, 3), c(1, 4, 9)) %>%
  eigen_ord() %>%
  as_tbl_ord() %>%
  negate_to_first_orthant("rows") ->
  ord

# adapted from tibble tests
test_that("`print()` returns object invisibly", {
  expect_output(ret <- withVisible(print(as_tbl_ord(ord))))
  expect_false(ret$visible)
  expect_identical(ret$value, as_tbl_ord(ord))
})

test_that("single values are recycled", {
  ord_fmt <- format(ord, n = 2)
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 4L)
})

test_that("lists with `NULL` are accepted", {
  # rows factor
  ord_fmt <- format(ord, n = list(NULL, 1))
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 3L + 1L)
  # columns factor
  ord_fmt <- format(ord, n = list(2, NULL))
  expect_equal(length(grep("[0-9]+\\.[0-9]+", ord_fmt)), 2L + 3L)
})
