# Reaven & Miller overt & chemical diabetes test data and group classification
head(heplots::Diabetes)
# default (standardized) linear discriminant analysis of groups on tests
diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes)

# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(diabetes_lda) %>%
  augment_ord() %>%
  print() -> diabetes_lda

# row-standard biplot
diabetes_lda %>%
  confer_inertia(1) %>%
  ggbiplot(aes(label = name), elements = "active") +
  theme_bw() + theme_biplot() +
  geom_rows_text() +
  geom_cols_vector(subset = c(1, 3, 4)) +
  geom_cols_text_radiate(subset = c(1, 3, 4), size = 3) +
  geom_cols_isoline(subset = c(1, 3, 4), alpha = .25, num = 4L,
                    label_dodge = -.03, text_alpha = .5, text_size = 3) +
  ggtitle(
    "LDA of Reaven & Miller diabetes groups",
    "Row-standard biplot of standardized LDA"
  )
