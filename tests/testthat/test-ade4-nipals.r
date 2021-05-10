context("NIPALS, class 'nipals' & 'nipals_ord'")

aq_sub <- airquality[airquality$Month == 6L, seq(4L)]
fit_ade4_nipals <- ade4::nipals(aq_sub)
fit_nipals_ord <- nipals_ord(aq_sub)

test_that("`as_tbl_ord()` coerces 'nipals' & 'nipals_ord' objects", {
  expect_true(valid_tbl_ord(as_tbl_ord(fit_ade4_nipals)))
  expect_equal(class(fit_nipals_ord), "nipals_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_nipals_ord)))
})
