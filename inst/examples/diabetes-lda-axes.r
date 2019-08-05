# LDA biplot scales for Reaven & Miller diabetes data
diabetes_custom_biplot <- function(axes.scale) {
  lda_ord(group ~ ., heplots::Diabetes, axes.scale = axes.scale) %>%
    as_tbl_ord() %>%
    augment() %>%
    print() %T>%
    mutate_u(discriminant = ifelse(! .supplement, "centroid", "case")) %>%
    confer_inertia("symmetric") %>%
    ggbiplot(.supplement = FALSE) +
    theme_bw() +
    geom_u_point(aes(shape = .grouping)) +
    geom_v_axis(color = "#888888") +
    geom_v_axis_ticks() +
    geom_v_axis_text(size = 2.5, label_dodge = .025) +
    ggtitle("Symmetric LDA biplot of Reaven & Miller diabetes data")
}
diabetes_custom_biplot("unstandardized")
diabetes_custom_biplot("standardized")
diabetes_custom_biplot("contribution")
