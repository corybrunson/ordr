context("classical multi-dimensional scaling, class 'cmds_ord")

fit_cmds <- cmdscale_ord(eurodist, k = 6L)

test_that("`as_tbl_ord()` coerces 'cmds_ord' objects", {
  expect_equal(class(fit_cmds), "cmds_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_cmds)))
})
