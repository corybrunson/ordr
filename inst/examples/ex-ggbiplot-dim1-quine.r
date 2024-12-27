data(quine, package = "MASS")
# 1-dimensional correspondence analysis
quine_df <- as.data.frame(unclass(table(quine[, c("Sex", "Lrn")])))
(quine_ca <- ordinate(quine_df, MASS::corresp))
# 1-dimensional biplots
quine_ca |> 
  ggbiplot(aes(x = 1)) +
  # TODO: Fix `y = 0` if only `aes(x = )` is passed.
  # geom_cols_vector() +
  geom_rows_bar(stat = "bin")
quine_ca |> 
  ggbiplot(aes(x = 1)) +
  # TODO: Coordinate vertical positions with respect to origin.
  geom_origin() +
  geom_rows_rug(sides = "t", color = "blue") +
  geom_cols_rug(sides = "b", color = "red")
