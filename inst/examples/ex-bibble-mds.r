# reproduce Exhibit 4.5 in Greenacre (2010)

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
(b <- as_bibble(m))
(d <- fortify(b))

# basic multidimensional scaling biplot
gg <- ggbiplot(b, aes(x = PCo1, y = PCo2)) +
  geom_v_text(aes(label = .name))
gg
# default aesthetic assignments
ggbiplot(b) +
  geom_v_text(aes(label = .name))
# numeric aesthetic assignments
ggbiplot(b, aes(x = 2, y = 1)) +
  geom_v_text(aes(label = .name))
# regress the attributes on the plotting dimensions and annotate the biplot
fit <- lm(x2 ~ get_u(b)) %>% as_bibble()
gg +
  geom_v_axis(data = fit, aes(label = .name))
gg +
  geom_v_axis(data = fortify(fit, include = "all"), aes(label = .name))
gg +
  geom_v_axis(data = fortify(fit, "v"), aes(label = .name))

# must add aesthetic `.matrix = NULL` to use new data without `.matrix` column
# (this should be remedied)
dat <- fortify(fit, "v")
dat$.matrix <- NULL
gg +
  geom_v_vector(data = fit) +
  geom_text(data = dat, aes(label = .name, .matrix = NULL))
