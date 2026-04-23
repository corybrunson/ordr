savings_pop <- LifeCycleSavings[, c("pop15", "pop75")]
savings_oec <- LifeCycleSavings[, c("sr", "dpi", "ddpi")]
savings_cca <- cancor_ord(savings_pop, savings_oec, scores = TRUE)

test_that("`get_rows()` and `get_cols()` obtain specified elements", {
  # `get_rows()`
  expect_identical(
    get_rows(savings_cca),
    get_rows(savings_cca, elements = "all")
  )
  expect_equal(
    nrow(get_rows(savings_cca, elements = "active")),
    ncol(savings_pop)
  )
  expect_equal(
    nrow(get_rows(savings_cca, elements = "score")),
    nrow(savings_pop)
  )
  expect_equal(
    nrow(get_rows(savings_cca, elements = "structure")),
    ncol(savings_pop)
  )
  expect_equal(nrow(get_rows(savings_cca, elements = "pinv_weight")), 0L)
  # `get_cols()`
  expect_identical(
    get_cols(savings_cca),
    get_cols(savings_cca, elements = "all")
  )
  expect_equal(
    nrow(get_cols(savings_cca, elements = "active")),
    ncol(savings_oec)
  )
  expect_equal(
    nrow(get_cols(savings_cca, elements = "score")),
    nrow(savings_oec)
  )
  expect_equal(
    nrow(get_cols(savings_cca, elements = "structure")),
    ncol(savings_oec)
  )
  expect_equal(nrow(get_cols(savings_cca, elements = "pinv_weight")), 0)
})

test_that("row and column recoverers use consistent axis names", {
  axis_names <- get_coord(savings_cca)
  # inertia
  expect_identical(names(get_inertia(savings_cca)), axis_names)
  # row elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_rows(savings_cca, elements = elts)),
      axis_names
    )
  }
  # column elements
  for (elts in .ord_elements) {
    expect_identical(
      colnames(get_cols(savings_cca, elements = elts)),
      axis_names
    )
  }
})
