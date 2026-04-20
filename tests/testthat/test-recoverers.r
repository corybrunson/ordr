savings_pop <- LifeCycleSavings[, c("pop15", "pop75")]
savings_oec <- LifeCycleSavings[, c("sr", "dpi", "ddpi")]
savings_cca <- cancor_ord(savings_pop, savings_oec, scores = TRUE)

test_that("`get_rows()` and `get_cols()` obtain specified elements", {
  # `get_rows()`
  expect_identical(
    get_rows(savings_cca),
    get_rows(savings_cca, elements = "all")
  )
  expect_equal(nrow(get_rows(savings_cca, elements = "active")), 2)
  expect_equal(nrow(get_rows(savings_cca, elements = "score")), 50)
  expect_equal(nrow(get_rows(savings_cca, elements = "structure")), 2)
  expect_equal(nrow(get_rows(savings_cca, elements = "pinv_weight")), 0)
  # `get_cols()`
  expect_identical(
    get_cols(savings_cca),
    get_cols(savings_cca, elements = "all")
  )
  expect_equal(nrow(get_cols(savings_cca, elements = "active")), 3)
  expect_equal(nrow(get_cols(savings_cca, elements = "score")), 50)
  expect_equal(nrow(get_cols(savings_cca, elements = "structure")), 3)
  expect_equal(nrow(get_cols(savings_cca, elements = "pinv_weight")), 0)
})
