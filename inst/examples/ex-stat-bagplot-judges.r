judges_pca <- ordinate(USJudgeRatings, prcomp)
# FIXME: Generated shortcuts don't preserve internal processes.
ggbiplot(judges_pca, sec.axes = "cols") +
  geom_rows_bagplot() +
  geom_cols_vector(aes(label = name))
