# Poisson regression of yarn weaving data
warpbreaks %>%
  glm(formula = breaks ~ wool + tension, family = "poisson") %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> warpbreaks_pois
# summarize ordination
glance(warpbreaks_pois)
# regression biplot with nonlinear isolines
warpbreaks_pois %>%
  ggbiplot(aes(
    x = tensionM, y = tensionH, intercept = `(Intercept)`, label = .name
  )) +
  theme_bw() + theme_biplot() +
  geom_jitter(stat = "rows", width = .1, height = .1) +
  geom_cols_vector() +
  geom_cols_text_radiate() +
  geom_cols_isolines(family = poisson(), by = 5)
