context("factor analysis, class 'factanal'")

fit_factanal <- factanal(swiss, factors = 3L, scores = "regression")
test_that("`as_tbl_ord()` coerces 'factanal' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_factanal)))
})
