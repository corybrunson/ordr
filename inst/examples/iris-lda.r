# Unstandardized, standardized, and de-sphered LDAs of Anderson Iris data
lda_biplot <- function(lda) {
  lda %>%
    as_tbl_ord() %>%
    augment() %>%
    mutate_u(discriminant = ifelse(! .supplement, "centroid", "case")) %>%
    ggbiplot() +
    theme_bw() +
    geom_u_point(aes(color = .grouping, size = discriminant), alpha = .5) +
    geom_v_vector(color = "#888888") +
    geom_v_text_radiate(aes(label = .name), size = 3) +
    scale_color_brewer(type = "qual", palette = 2)
}
MASS::lda(iris[, 1:4], iris[, 5]) %>%
  lda_biplot() +
  ggtitle("Unstandardized coefficient biplot of iris LDA")
lda_ord(iris[, 1:4], iris[, 5], axes.scale = "standardized") %>%
  lda_biplot() +
  ggtitle("Standardized coefficient biplot of iris LDA")
lda_ord(iris[, 1:4], iris[, 5], axes.scale = "desphered") %>%
  lda_biplot() +
  ggtitle("De-sphered coefficient biplot of iris LDA")
