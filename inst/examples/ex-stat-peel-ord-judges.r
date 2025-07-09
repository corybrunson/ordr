
# hull peeling with breaks below
judge_pca <- ordinate(USJudgeRatings, princomp, cols = -c(1, 12))
ggbiplot(judge_pca, axis.type = "predictive") +
  geom_cols_axis() +
  geom_rows_point(elements = "score") +
  stat_rows_peel(
    aes(alpha = after_stat(hull)), color = "black", elements = "score",
    breaks = c(.9, .5, .1), cut = "below"
  )

# hull peeling by groups
iris_pca <- ordinate(iris, cols = 1:4, model = prcomp)
ggbiplot(iris_pca) +
  geom_rows_point(aes(color = Species), shape = "circle open") +
  stat_rows_peel(
    aes(fill = Species, alpha = after_stat(hull)),
    num = 3
  )
