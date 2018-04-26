context("classical multi-dimensional scaling")

data(country_differences)
m <- country_differences %>%
  select(-Countries) %>%
  as.matrix()
fit_cmds <- cmdscale(m, k = 6)

test_that("'to_bibble()' coerces 'lm' objects", {
  expect_true(is.bibble(to_bibble(fit_cmds)))
})
