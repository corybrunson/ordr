library(ordr)
context("log-ratio analysis, class 'lra'")

fit_lra <- lra(USArrests[, -3])
test_that("`as_tbl_ord()` coerces 'lra' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_lra)))
})
