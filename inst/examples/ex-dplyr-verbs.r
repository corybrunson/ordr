
data(country_differences, country_attributes)
# format both tibbles as matrices
x1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}
x2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix() %>%
  {rownames(.) <- pull(country_differences, Countries); .}

# multidimensional scaling setup
(m <- cmdscale(x1, k = 2))
(a <- as_bibble(m))
(b <- bibble(m))
(d <- fortify(b))

# pull

#pull.bbl(a, matrix = "v", name)
#pull(a, matrix = "v", name)
pull_v(a, name)
#pull.bbl(a, name)

# select

# rename

# mutate

mutate(a, matrix = "u", abbr = pull(get_v(a), name))
mutate_u(a, abbr = pull_v(a, name))

mutate(a, abbr = pull(get_v(a), name))
mutate(a, abbr = abbreviate(name))

# transmute

# inner join: one matrix
inner_join(a, matrix = "u", country_attributes, by = c("name" = "Countries"))
inner_join_u(a, country_attributes, by = c("name" = "Countries"))
# inner join: failure (wrong matrix)
inner_join(a, matrix = "v", country_attributes, by = c("name" = "Countries"))
# inner join: both matrices
inner_join(a, rename(country_attributes, name = Countries))
