fit_cmds <- cmdscale_ord(eurodist, k = 6L)

test_that("'cmds_ord' accessors have consistent dimensions", {
  expect_equal(ncol(get_rows(fit_cmds)), ncol(get_cols(fit_cmds)))
  expect_equal(ncol(get_rows(fit_cmds)), length(recover_inertia(fit_cmds)))
})

test_that("'cmds_ord' has specified distribution of inertia", {
  expect_type(recover_conference(fit_cmds), "double")
  expect_vector(recover_conference(fit_cmds), size = 2L)
})

test_that("'cmds_ord' augmentations are consistent with '.element' column", {
  expect_equal(".element" %in% names(recover_aug_rows(fit_cmds)),
               ".element" %in% names(recover_aug_cols(fit_cmds)))
})

test_that("`as_tbl_ord()` coerces 'cmds_ord' objects", {
  expect_equal(class(fit_cmds), "cmds_ord")
  expect_true(valid_tbl_ord(as_tbl_ord(fit_cmds)))
})
