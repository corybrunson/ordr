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
(a <- as_bibble(m))
(b <- bibble(m))
(d <- tidy(b))

# basic multidimensional scaling biplot
gg <- ggbiplot(b, aes(x = PCo1, y = PCo2)) +
  geom_v_text(aes(label = name))
gg
# regress the attributes on the plotting dimensions and annotate the biplot
fit <- lm(x2 ~ factor_u(b)) %>% as_bibble()
gg +
  geom_v_vector(data = fit) +
  geom_v_text(data = tidy(fit, include = "all"), aes(label = name))
# hopeful alternative calls to produce the same annotation
\dontrun{
gg +
  geom_v_vector(data = fit) +
  geom_v_text(data = fit, aes(label = name))
# need to investigate further
gg +
  geom_v_vector(data = fit) +
  geom_text(data = get_v(fit), aes(label = name))
# 'geom_text' is fed 'stat = .matrix' and doesn't know what to do with it
}
