# Linear discriminant analysis of Reaven & Miller diabetes data
head(heplots::Diabetes)
diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes)
print(diabetes_lda)
as_tbl_ord(diabetes_lda) %>%
  augment() %>%
  mutate_u(discriminant = ifelse(! .supplement, "centroid", "case")) %>%
  print() -> diabetes_lda
ggbiplot(diabetes_lda) +
  theme_bw() +
  geom_u_point(aes(shape = .grouping, size = discriminant), alpha = .5) +
  geom_v_axis(color = "#888888") +
  ggtitle("Column-standardized LDA biplot of Reaven & Miller diabetes data")
diabetes_lda %>%
  confer_inertia("symmetric") %>%
  ggbiplot(.supplement = FALSE) +
  theme_bw() +
  geom_u_point(aes(shape = .grouping)) +
  geom_v_axis(color = "#888888") +
  #geom_v_ticks() +
  ggtitle("Symmetric LDA biplot of Reaven & Miller diabetes data")
