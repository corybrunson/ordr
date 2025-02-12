# Freestone primary glass measurements
head(glass)
# default (standardized) linear discriminant analysis of sites on measurements
glass_lda <- MASS::lda(Site ~ SiO2 + Al2O3 + FeO + MgO + CaO, glass)

# bestow 'tbl_ord' class & augment centroid and variable fields
as_tbl_ord(glass_lda) %>%
  augment_ord() %>%
  print() -> glass_lda

# row-standard biplot
glass_lda %>%
  confer_inertia(1) %>%
  ggbiplot(aes(label = name), elements = "active") +
  theme_bw() + theme_biplot() +
  geom_rows_text() +
  geom_cols_vector(subset = c(1, 3, 4), size = 3) +
  geom_cols_isoline(subset = c(1, 3, 4), alpha = .25, num = 4L,
                    text_dodge = -.03, text.alpha = .5, text.size = 3) +
  ggtitle(
    "LDA of Freestone glass measurements",
    "Row-standard biplot of standardized LDA"
  ) +
  scale_x_continuous(expand = expansion(mult = .1)) +
  scale_y_continuous(expand = expansion(mult = .1))
