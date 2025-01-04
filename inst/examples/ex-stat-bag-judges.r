judge_pca <- ordinate(USJudgeRatings, princomp)
ggbiplot(judge_pca, axis.type = "predictive") +
  geom_cols_axis() +
  geom_rows_point(elements = "score") +
  stat_rows_bag(
    aes(alpha = after_stat(hull)), color = "black", elements = "score",
    fraction = c(.9, .5, .1)
  )
ggbiplot(judge_pca, axis.type = "predictive") +
  geom_cols_axis() +
  geom_rows_point(elements = "score") +
  stat_rows_bag(
    aes(alpha = after_stat(hull)), color = "black", elements = "score",
    fraction = c(.9, .5, .1), cut = "below"
  )

iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)
ggbiplot(iris_pca) +
  geom_rows_point(aes(color = Species), shape = "circle open") +
  stat_rows_bag(
    aes(fill = Species, alpha = after_stat(hull)),
    fraction = c(.9, .5, .1)
  )
