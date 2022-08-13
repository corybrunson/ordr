# Reaven & Miller overt & chemical diabetes test data and group classification
head(heplots::Diabetes)

# default (standardized) linear discriminant analysis of groups on tests
diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes)
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(diabetes_lda) %>%
  augment_ord() %>%
  mutate_rows(discriminant = ifelse(
    .element == "active",
    "centroid", "case"
  )) %>%
  print() -> diabetes_lda
# row-standard biplot
diabetes_lda %>%
  confer_inertia(1) %>%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_rows_point(aes(shape = grouping, size = discriminant), alpha = .5) +
  geom_cols_axis(aes(label = name), color = "#888888", num = 8L,
                 text_size = 2.5, label_dodge = .02) +
  ggtitle(
    "LDA of Reaven & Miller diabetes groups",
    "Row-standard biplot of standardized LDA"
  )

# contribution LDA of groups on tests
diabetes_lda <-
  lda_ord(group ~ ., heplots::Diabetes, axes.scale = "contribution")
# bestow 'tbl_ord' class & augment observation, centroid, and variable fields
as_tbl_ord(diabetes_lda) %>%
  augment_ord() %>%
  mutate_rows(discriminant = ifelse(
    .element == "active",
    "centroid", "case"
  )) %>%
  print() -> diabetes_lda
# symmetric biplot
diabetes_lda %>%
  confer_inertia(.5) %>%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_rows_point(aes(shape = grouping, alpha = discriminant)) +
  geom_cols_axis(color = "#888888", num = 8L,
                 text_size = 2.5, text_dodge = .025) +
  ggtitle(
    "LDA of Reaven & Miller diabetes groups",
    "Symmetric biplot of contribution LDA"
  )
