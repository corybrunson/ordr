
# default (standardized) linear discriminant analysis
glass_lda <- MASS::lda(Site ~ SiO2 + Al2O3 + FeO + MgO + CaO, glass)
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(glass_lda) %>%
  augment_ord() %>%
  print() -> glass_lda
# row-standard biplot
glass_lda %>%
  confer_inertia(1) %>%
  ggbiplot(aes(shape = grouping)) +
  theme_bw() + theme_biplot() +
  geom_rows_point(size = 4) +
  geom_rows_point(elements = "score") +
  stat_cols_rule(
    aes(label = name), color = "#888888", num = 8L,
    ref_elements = "score", fun.offset = function(x) minabspp(x, p = .1),
    text.size = 2.5, label_dodge = .04
  ) +
  scale_shape_manual(values = c(2L, 3L, 0L, 5L)) +
  ggtitle(
    "LDA of Freestone glass measurements",
    "Row-standard biplot of standardized LDA"
  )
# contribution LDA of sites on measurements
glass_lda <-
  lda_ord(Site ~ SiO2 + Al2O3 + FeO + MgO + CaO, glass,
          axes.scale = "contribution")
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(glass_lda) %>%
  augment_ord() %>%
  print() -> glass_lda
# symmetric biplot
glass_lda %>%
  confer_inertia(.5) %>%
  ggbiplot(aes(shape = grouping)) +
  theme_bw() + theme_biplot() +
  geom_rows_point() +
  stat_rows_density_2d(elements = "score", alpha = .5, color = "#444444") +
  stat_cols_rule(
    aes(label = name), geom = "axis", color = "#888888", num = 8L,
    ref_elements = "active", fun.offset = function(x) minabspp(x, p = .1),
    label_dodge = 0.04, text.size = 2.5, text_dodge = .025
  ) +
  scale_shape_manual(values = c(16L, 17L, 15L, 18L)) +
  ggtitle(
    "LDA of Freestone glass measurements",
    "Symmetric biplot of contribution LDA"
  )
