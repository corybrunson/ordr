# Logistic SVD of Sanderson finches data
finches %>% t() %>%
  logisticSVD() %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> finches_lsvd
finches_lsvd %>%
  ggbiplot(
    aes(x = LSC1, y = LSC2, label = .name),
    sec.axes = "v", scale.factor = 100
  ) +
  geom_u_vector(alpha = .5, color = "darkred") +
  geom_u_text_radiate(size = 3, color = "darkred") +
  geom_v_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Logistic SVD of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-80, 60))
# Logistic PCA of Sanderson finches data
finches %>% t() %>%
  logisticPCA() %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> finches_lpca
finches_lpca %>%
  ggbiplot(
    aes(x = LPC1, y = LPC2, label = .name),
    sec.axes = "v", scale.factor = 50
  ) +
  geom_u_vector(alpha = .5, color = "darkred") +
  geom_u_text_radiate(size = 3, color = "darkred") +
  geom_v_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Logistic PCA of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-30, 25))
# Convex logistic PCA of Sanderson finches data
finches %>% t() %>%
  # omit finches present on every island
  {.[, ! apply(., 2, all), drop = FALSE]} %>%
  convexLogisticPCA() %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> finches_clpca
finches_clpca %>%
  ggbiplot(
    aes(x = LPC1, y = LPC2, label = .name),
    sec.axes = "v", scale.factor = 50
  ) +
  geom_u_vector(alpha = .5, color = "darkred") +
  geom_u_text_radiate(size = 3, color = "darkred") +
  geom_v_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Convex logistic PCA of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-25, 35))
