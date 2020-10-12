# Linear regression on marine ecosystem data
# Adapt Exhibit 2.3 in Greenacre (2010)
data(bioenv)
bioenv %>%
  transform(
    x = as.vector(scale(Depth)),
    y = as.vector(scale(Pollution))
  ) %>%
  lm(formula = d ~ x + y) %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> bioenv_lm
bioenv_lm %>%
  ggbiplot(aes(x = x, y = y, color = .fit, alpha = .wt.res ^ 2)) +
  theme_bw() +
  scale_color_distiller(type = "div", palette = 1) +
  scale_alpha_continuous(range = c(1/3, 1)) +
  geom_u_point() +
  geom_v_vector() +
  geom_v_isolines(aes(intercept = `(Intercept)`), axes = 1, by = 5) +
  labs(x = "Standardized Depth", y = "Standardized Pollution") +
  ggtitle(
    "Gradient vector and contour lines for species 'd'",
    "Regression plane: standardized depth-pollution space"
  )
