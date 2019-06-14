# Generalized linear regression on marine ecosystem data
bioenv %>%
  transform(
    x = as.vector(scale(Depth)),
    y = as.vector(scale(Pollution))
  ) %>%
  glm(formula = d ~ x + y, family = "poisson") %>%
  as_tbl_ord() %>%
  print() -> bioenv_pois
bioenv_pois %>%
  ggbiplot(aes(x = x, y = y, intercept = `(Intercept)`, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4") +
  geom_v_isolines(family = poisson(), by = 2)
# Adapt (3.4) and Exhibit 3.6 in Greenacre (2010)
bioenv %>%
  dplyr::mutate_at(dplyr::vars(a:e), as.logical) %>%
  transform(
    x = as.vector(scale(Depth)),
    y = as.vector(scale(Pollution))
  ) %>%
  glm(formula = d ~ x + y, family = "binomial") %>%
  as_tbl_ord() %>%
  print() -> bioenv_bin
bioenv_bin %>%
  ggbiplot(aes(x = x, y = y, intercept = `(Intercept)`, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4") +
  geom_v_isolines(family = binomial(), by = .05)
