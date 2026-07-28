# Layout setup: resolve_n, get_ord_layout, ord_width_alloc, ord_n_show

# resolve_n -------------------------------------------------------------------

test_that("resolve_n handles scalar, vector, list, Inf, and NULL inputs", {
  expect_equal(resolve_n(2, c(150L, 4L)), c(2L, 2L))
  expect_equal(resolve_n(c(5L, 10L), c(150L, 4L)), c(5L, 10L))
  expect_equal(resolve_n(list(NULL, 3L), c(150L, 4L)), c(5L, 3L))
  expect_equal(resolve_n(Inf, c(150L, 4L)), c(150L, 4L))
  expect_equal(resolve_n(NULL, c(5L, 3L)), c(5L, 3L))
  expect_equal(resolve_n(NULL, c(150L, 4L)), c(5L, 4L))
})

# get_ord_layout --------------------------------------------------------------

test_that("get_ord_layout extracts basic info and respects arguments", {
  layout <- get_ord_layout(ord_pca)
  expect_equal(layout$width, getOption("width"))
  expect_equal(layout$rk, 4L)
  expect_equal(unname(layout$n_dims), c(150L, 4L))
  expect_equal(layout$coord, c("PC1", "PC2", "PC3", "PC4"))

  layout40 <- get_ord_layout(ord_pca, width = 40)
  expect_equal(layout40$width, 40L)

  layout5 <- get_ord_layout(ord_pca, max_extra_cols = 5)
  expect_equal(layout5$max_extra_cols, 5L)

  layout3 <- get_ord_layout(ord_pca, max_footer_lines = 3)
  expect_equal(layout3$max_footer_lines, 3L)
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

test_that("ord_width_alloc handles wide, narrow, and no-annotation layouts", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_equal(alloc$coord_avail, 48L)
  expect_equal(alloc$ann_avail, 25L)
  expect_equal(alloc$sep_width, 6L)
  expect_equal(alloc$has_ann, TRUE)

  layout_narrow <- get_ord_layout(ord_pca, width = 30)
  alloc_narrow <- ord_width_alloc(layout_narrow)
  expect_equal(alloc_narrow$coord_avail, 12L)
  expect_equal(alloc_narrow$ann_avail, 11L)
  expect_equal(alloc_narrow$sep_width, 6L)
  expect_equal(alloc_narrow$has_ann, TRUE)

  layout_small <- get_ord_layout(ord_small, width = 80)
  alloc_small <- ord_width_alloc(layout_small)
  expect_equal(alloc_small$has_ann, FALSE)
  expect_equal(alloc_small$sep_width, 0L)
  expect_equal(alloc_small$ann_avail, 0L)
})

test_that("alloc many coords", {
  x10 <- as_tbl_ord(make_tbl_ord(
    rows = matrix(1:30, 3, 10, dimnames = list(NULL, paste0("V", 1:10))),
    cols = matrix(1:30, 3, 10, dimnames = list(NULL, paste0("V", 1:10)))
  ))
  layout <- get_ord_layout(x10, width = 80)
  alloc <- ord_width_alloc(layout)
  expect_true(alloc$max_coord_cols > 3L)
})

# ord_n_show ------------------------------------------------------------------

test_that("ord_n_show preserves n from layout", {
  layout <- get_ord_layout(ord_pca, width = 80)
  alloc <- ord_width_alloc(layout)
  layout <- ord_n_show(layout, alloc)
  expect_equal(layout$n_show, layout$n)

  layout2 <- get_ord_layout(ord_pca, width = 80, n = c(3L, 5L))
  alloc2 <- ord_width_alloc(layout2)
  layout2 <- ord_n_show(layout2, alloc2)
  expect_equal(layout2$n_show, c(3L, 5L))
})
