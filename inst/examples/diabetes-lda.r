# Linear discriminant analysis of Reaven & Miller diabetes data
head(heplots::Diabetes)
(diabetes_lda <- MASS::lda(group ~ ., heplots::Diabetes))
dim(supplementation_u(diabetes_lda))
as_tbl_ord(diabetes_lda) %>%
  augment() %>%
  mutate_u(discriminant = ifelse(! .supplement, "centroid", "case")) %>%
  print() -> diabetes_lda
ggbiplot(diabetes_lda) +
  theme_bw() +
  geom_u_point(aes(color = .grouping, size = discriminant), alpha = .5) +
  geom_v_axis(color = "#888888") +
  geom_v_text_radiate(aes(label = .name)) +
  scale_color_brewer(type = "qual", palette = 2) +
  ggtitle(
    "Column-standardized LDA biplot of Reaven & Miller diabetes data"
  )
