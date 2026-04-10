# FA of Swiss social data
swiss_fa <-
  ordinate(swiss, model = factanal, factors = 2L, scores = "regression")
# active and supplementary elements
get_rows(swiss_fa, elements = "active")
head(get_rows(swiss_fa, elements = "score"))

# biplot using matrix stats and element filter
ggbiplot(swiss_fa) +
  stat_rows(elements = "score") +
  stat_cols(geom = "vector", aes(label = name))

# biplot using element filter and item selection
# (note that filter precedes selection)
ggbiplot(swiss_fa) +
  geom_rows_point(elements = "score") +
  geom_rows_label(aes(label = name), elements = "score", subset = c(1, 4, 18)) +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  geom_cols_vector(aes(label = name))
