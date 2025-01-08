judges_pca <- ordinate(USJudgeRatings, prcomp)
ggbiplot(judges_pca, sec.axes = "cols") +
  geom_rows_bagplot() +
  geom_cols_vector(aes(label = name))
