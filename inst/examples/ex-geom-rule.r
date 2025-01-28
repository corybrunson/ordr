USJudgeRatings |> 
  subset(select = -c(1, 12)) |> 
  prcomp(rank. = 2) |> 
  getElement("x") |> as.data.frame() |> 
  transform(name = rownames(USJudgeRatings)) ->
  judge_pca
USJudgeRatings |> 
  subset(select = c(CONT, RTEN)) |> 
  setNames(c("contacts", "recommendation")) ->
  judge_meta
lm(as.matrix(judge_meta) ~ as.matrix(judge_pca[, seq(2)])) |> 
  getElement("coefficients") |> 
  unname() |> t() |> as.data.frame() |> 
  setNames(c("Intercept", "PC1", "PC2")) |> 
  transform(variable = names(judge_meta)) ->
  judge_lm
ggplot(judge_pca, aes(x = PC1, y = PC2)) +
  coord_equal() +
  theme_void() +
  geom_text(aes(label = name), size = 3) +
  stat_rule(
    data = judge_lm, referent = judge_pca,
    aes(center = Intercept, label = variable)
  )
# NB: `geom_rule(stat = "rule")` would fail to pass positional aesthetics.
