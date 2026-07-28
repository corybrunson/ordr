test_that("ord_combine alignment", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  expect_true(length(combined$rows) > 0L)
  expect_true(length(combined$cols) > 0L)
  # Each combined line has exactly one separator
  for (line in combined$rows) {
    expect_equal(length(gregexpr(" \\| ", line)[[1]]), 1L)
  }
  for (line in combined$cols) {
    expect_equal(length(gregexpr(" \\| ", line)[[1]]), 1L)
  }
})

test_that("ord_combine no annotations still includes separator", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  # Each line still has exactly one separator when annotations are empty
  for (line in combined$rows) {
    expect_equal(length(gregexpr(" \\| ", line)[[1]]), 1L)
  }
  for (line in combined$cols) {
    expect_equal(length(gregexpr(" \\| ", line)[[1]]), 1L)
  }
})

test_that("ord_combine padding", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  # rows: 7 coord lines (2 header + 5 data), 5 ann lines -> padded to 7
  expect_equal(length(combined$rows), 7L)
  # cols: 6 coord lines (2 header + 4 data), 4 ann lines -> padded to 6
  expect_equal(length(combined$cols), 6L)
})

test_that("ord_combine snapshot default", {
  layout <- get_ord_layout(ord_pca, width = 80, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  expect_snapshot(combined)
})

test_that("ord_combine snapshot narrow", {
  layout <- get_ord_layout(ord_pca, width = 30, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  expect_snapshot(combined)
})

test_that("ord_combine snapshot wide", {
  layout <- get_ord_layout(ord_pca, width = 120, n = 5L)
  alloc <- ord_width_alloc(layout)
  fc <- ord_format_coord(layout, alloc)
  sc <- ord_split_coord(fc, layout)
  ann <- ord_format_ann(layout, alloc)
  combined <- ord_combine(sc, ann, layout, alloc)
  expect_snapshot(combined)
})
