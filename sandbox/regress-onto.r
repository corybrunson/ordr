
data(country_differences, country_attributes)
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

b1 <- to_bibble(cmdscale(x1, k = 2))

bbl <- b1
newdata <- x2
# check that 'newdata' is compatible with u (or v if so specified)
stopifnot(nrow(newdata) == nrow(get_u(bbl)))
if (!is.null(rownames(newdata)))
  stopifnot(all(rownames(newdata) == get_u(bbl)$name))
fit <- lm(newdata ~ matrix_u(bbl))
make_bibble(
  u = get_u(bbl),
  v = bind_rows(
    mutate(get_v(bbl), .source = ".original"),
    mutate(get_v(fit), .source = ".regressed")
  ),
  coordinates = inner_join(
    get_coordinates(bbl),
    get_coordinates(fit),
    by = ".name", suffix = c(".original", ".regressed")
  )
)
