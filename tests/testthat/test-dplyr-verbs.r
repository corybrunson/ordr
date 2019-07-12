library(ordr)
context("dplyr verbs for ordination factors")

# arbitrary ordination object
pca <- augment(as_tbl_ord(prcomp(iris[, -5])))

test_that("`pull_*()` returns a vector", {
  expect_equal(pull_v(pca, .name), names(iris)[-5])
})

test_that("`bind_cols_*()` appends a column only of the correct length", {
  expect_equal(
    ncol(tidy(bind_cols_u(pca, iris[, 5, drop = FALSE]), .matrix = "u")),
    4 + 1 + 1
  )
  expect_equal(
    ncol(tidy(bind_cols_u(pca, species = iris[[5]]), .matrix = "u")),
    4 + 1 + 1
  )
  expect_error(bind_cols_u(pca, letter = letters), "length")
})

pca <- bind_cols_u(pca, iris[, 5, drop = FALSE])

test_that("`rename_*()` applies to both augmented and annotated variables", {
  expect_named(annotation_u(rename_u(pca, species = Species)), "species")
  expect_named(
    annotation_v(rename_v(pca, measure = .name)),
    c("measure", ".center")
  )
})

test_that("`rename_*()` does not recognize shared coordinates", {
  expect_error(rename_u(pca, dim1 = PC1))
})

test_that("`select()` applies to both augmented and annotated variables", {
  expect_false("Species" %in% names(tidy(select_u(pca, -Species))))
  expect_true("species" %in% names(tidy(select_u(pca, species = Species))))
})

test_that("`select_*()` does not recognize shared coordinates", {
  expect_error(select_u(pca, PC1))
})

test_that("`mutate_*()` recognizes but does not change shared coordinates", {
  expect_named(
    annotation_u(mutate_u(pca, Species = toupper(Species))),
    "Species"
  )
  expect_named(
    annotation_v(mutate_v(pca, diff = PC1 - .center)),
    c(".name", ".center", "diff")
  )
})

test_that("`transmute_*()` recognizes but does not change shared coordinates", {
  expect_named(
    annotation_u(transmute_u(pca, species = toupper(Species))),
    "species"
  )
  expect_named(annotation_v(transmute_v(pca, diff = PC1 - .center)), "diff")
})

means <- aggregate(iris[, -5], iris[, 5, drop = FALSE], mean)

test_that("`left_join_*()` joins new columns", {
  expect_named(
    annotation_u(left_join_u(pca, means, by = "Species")),
    c("Species", names(iris)[-5])
  )
})
