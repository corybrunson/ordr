USJudgeRatings |> 
  ordinate(prcomp) |> 
  mutate_rows(surname = gsub("(,|\\.).*$", "", name)) ->
  judges_pca
ggbiplot(judges_pca, sec.axes = "cols") +
  geom_rows_bagplot() +
  geom_rows_text(aes(label = surname), size = 2) +
  geom_cols_vector(aes(label = name), size = 3, alpha = .5)
