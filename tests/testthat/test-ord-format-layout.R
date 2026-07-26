# Layout setup: resolve_n, get_ord_layout, ord_width_alloc, ord_n_show

# resolve_n -------------------------------------------------------------------

test_that("resolve_n handles single value", {
  expect_equal(resolve_n(2, c(150L, 4L)), c(2L, 2L))
})

test_that("resolve_n handles vector of 2", {
  expect_equal(resolve_n(c(5L, 10L), c(150L, 4L)), c(5L, 10L))
})

test_that("resolve_n handles list with NULL", {
  # print_min is 5, so NULL for 150 rows gives 5
  expect_equal(resolve_n(list(NULL, 3L), c(150L, 4L)), c(5L, 3L))
})

test_that("resolve_n handles Inf", {
  expect_equal(resolve_n(Inf, c(150L, 4L)), c(150L, 4L))
})

test_that("resolve_n defaults when NULL", {
  expect_equal(resolve_n(NULL, c(5L, 3L)), c(5L, 3L))
})

test_that("resolve_n defaults when n_dims > print_max", {
  expect_equal(resolve_n(NULL, c(150L, 4L)), c(5L, 4L))
})

# get_ord_layout --------------------------------------------------------------

test_that("get_ord_layout extracts basic info", {
  layout <- get_ord_layout(ord_pca)
  expect_equal(layout$width, getOption("width"))
  expect_equal(layout$rk, 4L)
  expect_equal(unname(layout$n_dims), c(150L, 4L))
  expect_equal(layout$coord, c("PC1", "PC2", "PC3", "PC4"))
})

test_that("get_ord_layout respects width argument", {
  layout <- get_ord_layout(ord_pca, width = 40)
  expect_equal(layout$width, 40L)
})

test_that("get_ord_layout stores max_extra_cols", {
  layout <- get_ord_layout(ord_pca, max_extra_cols = 5)
  expect_equal(layout$max_extra_cols, 5L)
})

test_that("get_ord_layout stores max_footer_lines", {
  layout <- get_ord_layout(ord_pca, max_footer_lines = 3)
  expect_equal(layout$max_footer_lines, 3L)
})

test_that("get_ord_layout snapshot", {
  layout <- get_ord_layout(ord_pca, width = 80)
  info <- c(
    width = layout$width,
    rk = layout$rk,
    n_dims_rows = layout$n_dims[["rows"]],
    n_dims_cols = layout$n_dims[["cols"]],
    n_ann_rows = layout$n_ann[["rows"]],
    n_ann_cols = layout$n_ann[["cols"]],
    n_rows = layout$n[[1L]],
    n_cols = layout$n[[2L]],
    conference = paste(layout$conference, collapse = ","),
    prev_class = layout$prev_class %||% "NULL"
  )
  expect_snapshot(info)
})

# ord_width_alloc -------------------------------------------------------------

test_that("alloc wide", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_equal(alloc$coord_avail, 48L)
  expect_equal(alloc$ann_avail, 25L)
  expect_equal(alloc$sep_width, 6L)
  expect_equal(alloc$has_ann, TRUE)
})

test_that("alloc narrow", {
  layout <- get_ord_layout(ord_pca, width = 30)
  alloc <- ord_width_alloc(layout)
  expect_equal(alloc$coord_avail, 12L)
  expect_equal(alloc$ann_avail, 11L)
  expect_equal(alloc$sep_width, 6L)
  expect_equal(alloc$has_ann, TRUE)
})

test_that("alloc no annotations", {
  layout <- get_ord_layout(ord_small, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_equal(alloc$has_ann, FALSE)
  expect_equal(alloc$sep_width, 0L)
  expect_equal(alloc$ann_avail, 0L)
})

test_that("alloc many coords", {
  # Create layout with 10 coords
  x10 <- as_tbl_ord(make_tbl_ord(
    rows = matrix(1:30, 3, 10, dimnames = list(NULL, paste0("V", 1:10))),
    cols = matrix(1:30, 3, 10, dimnames = list(NULL, paste0("V", 1:10)))
  ))
  layout <- get_ord_layout(x10, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_true(alloc$max_coord_cols > 3L)
})

# ord_n_show ------------------------------------------------------------------

test_that("n_show default", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  layout <- ord_n_show(layout, alloc)
  expect_equal(layout$n_show, layout$n)
})

test_that("n_show explicit", {
  layout <- get_ord_layout(ord_pca, width = 80, n = c(3L, 5L))
  alloc <- ord_width_alloc(layout)
  layout <- ord_n_show(layout, alloc)
  expect_equal(layout$n_show, c(3L, 5L))
})
