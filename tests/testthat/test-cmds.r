library(tidybiplot)
context("classical multi-dimensional scaling")

data(country_differences)
m <- country_differences %>%
  dplyr::select(-Countries) %>%
  as.matrix()
fit_cmds <- cmdscale(m, k = 6)

test_that("`as_tbl_ord()` coerces 'cmds' objects", {
  expect_true(is.tbl_ord(as_tbl_ord(fit_cmds)))
})
