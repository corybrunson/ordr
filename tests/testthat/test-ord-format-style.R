# check if crayon actually produces ANSI codes in this environment
has_ansi_support <- function() {
  old <- options(crayon.enabled = TRUE)
  on.exit(options(old), add = TRUE)
  x <- cli::col_grey("test")
  grepl("\033", x, fixed = TRUE)
}

test_that("`style_subtle()` returns plain text (crayon off) or ANSI (on)", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  expect_equal(style_subtle("hello"), "hello")
})

test_that("`style_subtle()` returns ANSI-styled text when crayon is on", {
  skip_if_not(
    has_ansi_support(),
    "crayon does not produce ANSI in this environment"
  )
  out <- style_subtle("hello")
  expect_true(grepl("\033", out, fixed = TRUE))
  expect_false(grepl("3m", out, fixed = TRUE))
})

test_that("`format()` styles supplementary values in the subtle grey", {
  old <- options(crayon.enabled = TRUE, crayon.colors = 256L,
               cli.num_colors = 256L)
  on.exit(options(old), add = TRUE)
  out <- format(ord_lda, n = 5L, width = 100)
  # active data lines carry the subtle grey only in their row-number prefixes
  # and separators, i.e. as few styled spans...
  active_lines <- out[grepl("active", out, fixed = TRUE) &
                        ! grepl("score", out, fixed = TRUE)]
  expect_true(length(active_lines) > 0L)
  # ...supplementary rows carry it additionally in their values, so the gap
  # between their raw and stripped lengths exceeds any active line's
  overhead <- function(x) {
    nchar(x, type = "bytes") - nchar(strip_style(x), type = "bytes")
  }
  max_active <- max(overhead(active_lines))
  score_lines <- out[grepl("score", out, fixed = TRUE)]
  expect_true(length(score_lines) > 0L)
  expect_true(all(overhead(score_lines) > max_active))
})

test_that("supplementary dimming does not alter text content", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  plain <- format(ord_lda, n = 5L, width = 100)
  old2 <- options(crayon.enabled = TRUE, crayon.colors = 256L,
                  cli.num_colors = 256L)
  on.exit(options(old2), add = TRUE)
  colored <- format(ord_lda, n = 5L, width = 100)
  # {pillar} renders character NAs (`<NA>` vs. `NA`) and pads around them
  # differently depending on colour support, so compare normalized tokens
  norm <- function(x) {
    x <- strip_style(x)
    x <- gsub("<NA>", "NA", x, fixed = TRUE)
    gsub("\\s+", " ", trimws(x))
  }
  expect_equal(norm(colored), norm(plain))
})

test_that("`style_type()` returns plain text (crayon off) or ANSI (on)", {
  old <- options(crayon.enabled = FALSE)
  on.exit(options(old), add = TRUE)
  expect_equal(style_type("<dbl>"), "<dbl>")
})

test_that("`style_type()` returns italic+grey text when crayon is on", {
  skip_if_not(
    has_ansi_support(),
    "crayon does not produce ANSI in this environment"
  )
  out <- style_type("<dbl>")
  expect_true(grepl("3m", out, fixed = TRUE))
  expect_true(grepl("90m", out, fixed = TRUE))
})

test_that("`format()` applies tibble-harmonized styling", {
  skip_if_not(
    has_ansi_support(),
    "crayon does not produce ANSI in this environment"
  )
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
