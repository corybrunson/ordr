
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

#pull.bbl(a, .matrix = "v", name)
#pull(a, .matrix = "v", name)
pull_v(a, name)
#pull.bbl(a, name)

# rename

rename(b, country = name)
b %>%
  rename_u(country = name) %>%
  rename_v(abbr = name)

# select

select(b, -name)

# mutate

mutate(a, abbr = pull(get_v(a), name), .matrix = "u")
mutate_u(a, abbr = pull_v(a, name))

# use `name` attribute of V as `abbr` attribute of U
mutate(a, abbr = pull(get_v(a), name))
# abbreviate `name` to `abbr` separately in U and V
mutate(a, abbr = abbreviate(name))

# transmute

transmute(a, lowername = tolower(name))

# inner join

# inner join: one matrix
inner_join(a, country_attributes, .matrix = "u", by = c("name" = "Countries"))
inner_join_u(a, country_attributes, by = c("name" = "Countries"))
# inner join: failure (fields don't match up)
inner_join(a, country_attributes, .matrix = "v", by = c("name" = "Countries"))
# inner join: both matrices
inner_join(
  mutate_v(rename_v(a, abbr = name), name = pull(get_u(a), name)),
  country_attributes,
  by = c("name" = "Countries")
)

# left join

left_join(a, country_attributes, .matrix = "u", by = c("name" = "Countries"))
