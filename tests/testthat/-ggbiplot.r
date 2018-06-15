library(tidybiplot)
context("grammatical biplotting")

data(country_differences)
m <- country_differences %>%
  select(-Countries) %>%
  as.matrix()
f <- cmdscale(m, k = 3)
b <- as_bibble(f)
n <- nrow(get_u(b))

test_that("`ggbiplot()` handles coordinate aesthetics", {
  expect(all(c("x", "y") %in% names(ggbiplot(b)$mapping)))
  expect(all(c("x", "y") %in% names(ggbiplot(b, aes(y = 3))$mapping)))
  expect_error(print(ggbiplot(b) + geom_u_point()),
               regexp = NA)
  expect_error(print(ggbiplot(b, aes(x = 2, y = 0)) + geom_u_point()),
               regexp = "select")
  expect_error(print(ggbiplot(b, aes(x = 2, y = 0 + 0)) + geom_u_point()),
               regexp = NA)
})
