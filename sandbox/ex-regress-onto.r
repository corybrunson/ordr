
data(country_differences, country_attributes)
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

(b1 <- as_bibble(cmdscale(x1, k = 2)))

regress_onto(b1, x2, factor = "u")
