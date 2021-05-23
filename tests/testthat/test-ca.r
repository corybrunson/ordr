context("correspondence analysis, class 'ca'")

fit_ca <- ca::ca(HairEyeColor[, , "Male"])
test_that("`as_tbl_ord()` coerces 'ca' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_ca)))
})
