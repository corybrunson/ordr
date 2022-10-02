# arbitrary ordination object
pca <- augment_ord(as_tbl_ord(prcomp(iris[, -5])))

test_that("`pull_*()` returns a vector", {
  expect_equal(pull_cols(pca, name), names(iris)[-5])
})

test_that("`cbind_*()` appends a column only of the correct length", {
  expect_equal(
    ncol(fortify(cbind_rows(pca, iris[, 5, drop = FALSE]), .matrix = "rows")),
    # original + bound + fortified (`.matrix` & `.supplement`)
    4L + 1L + 2L
  )
  expect_equal(
    ncol(fortify(cbind_rows(pca, species = iris[[5]]), .matrix = "rows")),
    # original + bound + fortified (`.matrix` & `.supplement`)
    4L + 1L + 2L
  )
  expect_error(cbind_rows(pca, letter = letters),
               regexp = "length|recycle",
               class = "vctrs_error_incompatible_size")
})

pca <- cbind_rows(pca, iris[, 5, drop = FALSE])

test_that("`rename_*()` applies to both augmented and annotated variables", {
  expect_named(annotation_rows(rename_rows(pca, species = Species)), "species")
  expect_named(
    annotation_cols(rename_cols(pca, measure = name)),
    c("measure", "center")
  )
})

test_that("`rename_*()` does not recognize shared coordinates", {
  expect_error(rename_rows(pca, dim1 = PC1))
})

test_that("`select()` applies to both augmented and annotated variables", {
  expect_false("Species" %in% names(fortify(select_rows(pca, -Species))))
  expect_true("species" %in% names(fortify(select_rows(pca, species = Species))))
})

test_that("`select_*()` does not recognize shared coordinates", {
  expect_error(select_rows(pca, PC1))
})

test_that("`mutate_*()` recognizes but does not change shared coordinates", {
  expect_named(
    annotation_rows(mutate_rows(pca, Species = toupper(Species))),
    "Species"
  )
  expect_named(
    annotation_cols(mutate_cols(pca, diff = PC1 - center)),
    c("name", "center", "diff")
  )
})

test_that("`transmute_*()` recognizes but does not change shared coordinates", {
  expect_named(
    annotation_rows(transmute_rows(pca, species = toupper(Species))),
    "species"
  )
  expect_named(annotation_cols(transmute_cols(pca, diff = PC1 - center)), "diff")
})

means <- aggregate(iris[, -5], iris[, 5, drop = FALSE], mean)

test_that("`left_join_*()` joins new columns", {
  expect_named(
    annotation_rows(left_join_rows(pca, means, by = "Species")),
    c("Species", names(iris)[-5])
  )
})
