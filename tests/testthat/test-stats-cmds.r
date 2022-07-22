fit_cmds <- cmdscale(eurodist, k = 6L, eig = TRUE, x.ret = TRUE)

test_that("tidiers handle `cmdscale()` output", {
  
  # tidy outputs
  tidy_points <- tidy(fit_cmds)
  tidy_eig <- tidy(fit_cmds, matrix = "eig")
  tidy_x <- tidy(fit_cmds, matrix = "x")
  
  # tibble class
  expect_equal(class(tidy_points), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(tidy_eig), c("tbl_df", "tbl", "data.frame"))
  expect_equal(class(tidy_x), c("tbl_df", "tbl", "data.frame"))
  
  # dimensions
  expect_equal(dim(tidy_points), c(21L, 7L))
  expect_equal(dim(tidy_eig), c(6L, 4L))
  expect_equal(dim(tidy_x), c(21L * 20L / 2L, 3L))
  
})
