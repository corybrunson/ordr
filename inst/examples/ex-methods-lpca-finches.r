# site-species data frame of Sanderson Galapagos finches data
data(finches, package = "cooccur")
class(finches)
finches[seq(6L), seq(6L)]
# logistic singular value decomposition
finches %>% t() %>%
  logisticSVD_ord() %>%
  as_tbl_ord() %>%
  print() -> finches_lsvd
# summarize ordination
glance(finches_lsvd)
# recover row and column singular vectors
get_rows(finches_lsvd)
get_cols(finches_lsvd)
# augment ordination with point names and main effect estimates
augment_ord(finches_lsvd)
# summarize artifical coordinates (inertia is undefined)
tidy(finches_lsvd)
# biplot (inertia is not defined and cannot be conferred)
finches_lsvd %>%
  augment_ord() %>%
  ggbiplot(aes(label = .name), sec.axes = "cols", scale.factor = 100) +
  theme_biplot() +
  geom_rows_vector(alpha = .5, color = "darkred") +
  geom_rows_text_radiate(size = 3, color = "darkred") +
  geom_cols_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Logistic SVD of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-80, 60))
# logistic principal components analysis
finches %>% t() %>%
  logisticPCA_ord() %>%
  as_tbl_ord() %>%
  print() -> finches_lpca
# summarize ordination
glance(finches_lpca)
# recover row and column singular vectors
get_rows(finches_lpca)
get_cols(finches_lpca)
# augment ordination with point names and main effect estimates
augment_ord(finches_lpca)
# summarize artifical coordinates (inertia is undefined)
tidy(finches_lpca)
# biplot (inertia is not defined and cannot be conferred)
finches_lpca %>%
  augment_ord() %>%
  ggbiplot(aes(label = .name), sec.axes = "cols", scale.factor = 50) +
  theme_biplot() +
  geom_rows_vector(alpha = .5, color = "darkred") +
  geom_rows_text_radiate(size = 3, color = "darkred") +
  geom_cols_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Logistic PCA of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-30, 25))
# convex logistic PCA, omitting ubiquitous finches
finches %>%
  dplyr::filter(dplyr::if_any(where(is.integer), ~ . == 0)) %>%
  t() %>%
  convexLogisticPCA_ord() %>%
  as_tbl_ord() %>%
  print() -> finches_clpca
# summarize ordination
glance(finches_clpca)
# recover row and column singular vectors
get_rows(finches_clpca)
get_cols(finches_clpca)
# augment ordination with point names and main effect estimates
augment_ord(finches_clpca)
# summarize artifical coordinates (inertia is undefined)
tidy(finches_clpca)
# biplot (inertia is not defined and cannot be conferred)
finches_clpca %>%
  augment_ord() %>%
  ggbiplot(aes(label = .name), sec.axes = "cols", scale.factor = 50) +
  theme_biplot() +
  geom_rows_vector(alpha = .5, color = "darkred") +
  geom_rows_text_radiate(size = 3, color = "darkred") +
  geom_cols_label(size = 3, alpha = .5, color = "royalblue3") +
  ggtitle(
    "Convex logistic PCA of the Galapagos island finches",
    "Islands (finches) scaled to the primary (secondary) axes"
  ) +
  expand_limits(x = c(-25, 35))
