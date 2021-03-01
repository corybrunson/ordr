# Linear discriminant analysis of Reaven & Miller diabetes data
head(heplots::Diabetes)
diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes)
print(diabetes_lda)
as_tbl_ord(diabetes_lda) %>%
  augment() %>%
  mutate_rows(discriminant = ifelse(! .supplement, "centroid", "case")) %>%
  print() -> diabetes_lda
ggbiplot(diabetes_lda) +
  theme_bw() +
  geom_rows_point(aes(shape = .grouping, size = discriminant), alpha = .5) +
  geom_cols_axis(color = "#888888") +
  ggtitle("Column-standardized LDA biplot of Reaven & Miller diabetes data")
