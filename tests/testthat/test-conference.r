iris_pca <- as_tbl_ord(prcomp(iris[, 1:4]))
arrests_lra <- as_tbl_ord(lra(USArrests[, c(1,2,4)]))

test_that("`confer_inertia()` recognizes matrix factor strings", {
  expect_equal(get_conference(confer_inertia(arrests_lra, "rows")), c(1, 0))
  expect_equal(get_conference(confer_inertia(arrests_lra, "columns")), c(0, 1))
  expect_equal(get_conference(confer_inertia(arrests_lra, "symm")), c(.5, .5))
})

test_that("balanced proportions do not trigger errors or warnings", {
  expect_error(confer_inertia(iris_pca, c(.4, .6)), NA)
})
test_that("unbalanced values trigger a warning", {
  expect_warning(confer_inertia(iris_pca, c(1, .75)), "balance")
})
test_that("non-proportions trigger an error", {
  expect_error(confer_inertia(arrests_lra, c(1.75, -.25)), "proportion")
})
