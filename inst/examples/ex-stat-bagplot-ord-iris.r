
iris_pca <- ordinate(iris, prcomp, cols = seq(4), scale. = TRUE)
# NB: Non-standard aesthetics are handled as in version > 3.5.1; see:
# https://github.com/tidyverse/ggplot2/issues/6191
# This prevents `scale_color_discrete(aesthetics = ...)` from synching them.
ggbiplot(iris_pca) +
  stat_rows_bagplot(
    aes(fill = Species),
    median_gp = list(color = sync()),
    fence_gp = list(linewidth = 0.25),
    outlier_gp = list(shape = "asterisk")
  ) +
  scale_color_discrete(name = "Species", aesthetics = c("color", "fill")) +
  geom_cols_vector(aes(label = name))
ggbiplot(iris_pca) +
  stat_rows_bagplot(
    aes(fill = Species, color = Species),
    median_gp = list(color = sync()),
    fence_gp = list(linewidth = 0.25),
    outlier_gp = list(shape = "asterisk")
  ) +
  geom_cols_vector(aes(label = name))
