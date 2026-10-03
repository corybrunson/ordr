pca <- confer_inertia(as_tbl_ord(prcomp(USPersonalExpenditure)), "symmetric")

test_that("`facet_ord()` replaces the coordinate mappings", {
  p <- ggbiplot(pca) + facet_ord()
  expect_identical(rlang::as_label(p$mapping$x), ".panel_x")
  expect_identical(rlang::as_label(p$mapping$y), ".panel_y")
  # the axis titles are suppressed, but can be overridden
  expect_null(p$labels$x)
  expect_null(p$labels$y)
  expect_error(print(ggbiplot(pca) + facet_ord() + geom_rows_point()),
               regexp = NA)
  expect_error(print(ggbiplot(pca) + facet_ord() + labs(x = "PC 1") +
                       geom_rows_point()),
               regexp = NA)
})

test_that("`facet_ord()` lays out one panel per ordered coordinate pair", {
  p <- ggbiplot(pca) + facet_ord()
  k <- length(attr(fortify(pca, .matrix = "both"), "coordinates"))
  layout <- ggplot_build(p)$layout$layout
  expect_equal(nrow(layout), k^2)
  expect_identical(as.character(layout$x), rep(c("PC1", "PC2", "PC3",
                                                "PC4", "PC5"), times = k))
  expect_identical(as.character(layout$y), rep(c("PC1", "PC2", "PC3",
                                                "PC4", "PC5"), each = k))
  expect_identical(as.integer(layout$ROW), rep(seq_len(k), each = k))
  expect_identical(as.integer(layout$COL), rep(seq_len(k), times = k))
  # with fixed scales, all panels share a single scale per axis
  expect_true(all(layout$SCALE_X == 1L))
  expect_true(all(layout$SCALE_Y == 1L))
})

test_that("`facet_ord()` supports free scales", {
  p <- ggbiplot(pca) + facet_ord(scales = "free")
  k <- length(attr(fortify(pca, .matrix = "both"), "coordinates"))
  layout <- ggplot_build(p)$layout$layout
  expect_identical(as.integer(layout$SCALE_X), rep(seq_len(k), times = k))
  expect_identical(as.integer(layout$SCALE_Y), rep(seq_len(k), each = k))
})

test_that("`facet_ord()` couples `scales` and `space`", {
  k <- length(attr(fortify(pca, .matrix = "both"), "coordinates"))
  # neither set: both default to "fixed"
  expect_false(facet_ord()$params$free$x)
  expect_false(facet_ord()$params$space_free$x)
  # either one set: the other takes its value
  expect_true(facet_ord(scales = "free")$params$free$x)
  expect_true(facet_ord(scales = "free")$params$space_free$x)
  expect_true(facet_ord(space = "free")$params$free$x)
  expect_true(facet_ord(space = "free")$params$space_free$x)
  # both set: both values are retained
  expect_false(facet_ord(scales = "fixed", space = "free")$params$free$x)
  expect_true(facet_ord(scales = "fixed", space = "free")$params$space_free$x)
  expect_true(facet_ord(scales = "free", space = "fixed")$params$free$x)
  expect_false(facet_ord(scales = "free", space = "fixed")$params$space_free$x)
  # the coupled scales are used when the plot is built
  layout <- ggplot_build(ggbiplot(pca) + facet_ord(scales = "free"))$
    layout$layout
  expect_identical(as.integer(layout$SCALE_X), rep(seq_len(k), times = k))
  expect_identical(as.integer(layout$SCALE_Y), rep(seq_len(k), each = k))
})

test_that("`facet_ord()` restricts to the first coordinates", {
  p <- ggbiplot(pca) + facet_ord(num_coord = 2)
  layout <- ggplot_build(p)$layout$layout
  expect_equal(nrow(layout), 4L)
  expect_true(all(c("PC1", "PC2") %in% as.character(layout$x)))
  expect_false("PC3" %in% as.character(layout$x))
})

test_that("`facet_ord()` validates its arguments", {
  expect_error(facet_ord(num_coord = 0))
  expect_error(facet_ord(num_coord = "a"))
  expect_error(facet_ord(scales = "both"))
  expect_error(facet_ord(space = "both"))
})

test_that("`facet_ord()` requires shared coordinates", {
  p <- ggplot(mtcars, aes(mpg, hp)) + geom_point() + facet_ord()
  expect_error(print(p), regexp = "shared coordinates")
  # fall back to numeric columns shared by both matrix factors
  df <- data.frame(
    .matrix = c("rows", "rows", "cols", "cols"),
    a = 1:4, b = c(2, 1, 4, 3), .label = letters[1:4]
  )
  facet <- facet_ord()
  params <- facet$setup_params(list(df), facet$params)
  expect_identical(params$coords, c("a", "b"))
})
