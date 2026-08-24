# Check if crayon actually produces ANSI codes in this environment
has_ansi_support <- function() {
  old <- options(crayon.enabled = TRUE)
  on.exit(options(old), add = TRUE)
  x <- cli::col_grey("test")
  grepl("\033", x, fixed = TRUE)
}

test_that("style_subtle returns plain text when crayon is off, ANSI when on", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  expect_equal(style_subtle("hello"), "hello")
})

test_that("style_subtle returns ANSI-styled text when crayon is on", {
  skip_if_not(has_ansi_support(), "crayon does not produce ANSI in this environment")
  out <- style_subtle("hello")
  expect_true(grepl("\033", out, fixed = TRUE))
  expect_false(grepl("3m", out, fixed = TRUE))
})

test_that("style_type returns plain text when crayon is off, ANSI when on", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  expect_equal(style_type("<dbl>"), "<dbl>")
})

test_that("style_type returns italic+grey text when crayon is on", {
  skip_if_not(has_ansi_support(), "crayon does not produce ANSI in this environment")
  out <- style_type("<dbl>")
  expect_true(grepl("3m", out, fixed = TRUE))
  expect_true(grepl("90m", out, fixed = TRUE))
})

test_that("format applies tibble-harmonized styling", {
  skip_if_not(has_ansi_support(), "crayon does not produce ANSI in this environment")
  out <- format(ord_pca, n = 5L)
  # All lines should be styled
  expect_true(all(grepl("\033", out, fixed = TRUE)))
  # Header lines (starting with #) should be grey (not italic)
  hdr <- out[grepl("^#", out)]
  if (length(hdr) > 0L) {
    expect_true(all(grepl("90m", hdr, fixed = TRUE)))
    expect_false(any(grepl("3m", hdr, fixed = TRUE)))
  }
  # Types lines should be italic+grey
  types_lines <- out[grepl("^ +<", out)]
  if (length(types_lines) > 0L) {
    expect_true(all(grepl("3m", types_lines, fixed = TRUE)))
    expect_true(all(grepl("90m", types_lines, fixed = TRUE)))
  }
  # Data lines should have grey row numbers
  data_lines <- out[grepl("^[0-9]+ ", out)]
  if (length(data_lines) > 0L) {
    expect_true(all(grepl("90m", data_lines, fixed = TRUE)))
  }
  # Pipe separators should be grey
  pipe_lines <- out[grepl("[|]", out)]
  if (length(pipe_lines) > 0L) {
    expect_true(all(grepl("90m", pipe_lines, fixed = TRUE)))
  }
})
