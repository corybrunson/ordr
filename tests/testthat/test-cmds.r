library(tidybiplot)
context("classical multi-dimensional scaling")

data(country_differences)
m <- country_differences %>%
  dplyr::select(-Countries) %>%
  as.matrix()
fit_cmds <- cmdscale(m, k = 6)

test_that("`as_bibble()` coerces 'cmds' objects", {
  expect_true(is.bibble(as_bibble(fit_cmds)))
})
