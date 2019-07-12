library(ordr)
context("classical multi-dimensional scaling, class 'cmds")

data(country_differences)
fit_cmds <- cmdscale(country_differences, k = 6)

test_that("`as_tbl_ord()` coerces 'cmds' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_cmds)))
})
