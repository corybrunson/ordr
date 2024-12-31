# 1-dimensional correspondence analysis
(warp_ca <- ordinate(xtabs(breaks ~ ., warpbreaks), MASS::corresp))
# 2-dimensional biplot is incompatible
warp_ca |> 
  ggbiplot(aes(x = 1, y = 2)) +
  geom_rows_point()
# 1-dimensional biplots
warp_ca |> 
  ggbiplot(aes(x = 1)) +
  # TODO: Fix `y = 0` if only `aes(x = )` is passed.
  # geom_cols_vector() +
  geom_rows_bar(stat = "bin")
# NOTE: `GeomRug` is not designed for making use of both sides of the abscissa.
warp_ca |>
  ggbiplot(aes(x = 1)) +
  # TODO: Coordinate vertical positions with respect to origin.
  geom_origin() +
  geom_rows_rug(sides = "t", color = "blue") +
  geom_cols_rug(sides = "b", color = "red")

iris_pca <- ordinate(iris, cols = 1:4, model = ~ prcomp(., scale. = TRUE))
# TODO: Have secondary axis specification determine ordinate reversal.
iris_pca |> 
  ggbiplot(aes(x = 1), sec.axes = "cols", scale.factor = 10) +
  geom_vline(xintercept = 0) +
  geom_rows_density_2d(aes(color = Species)) +
  geom_cols_bar(aes(y = -after_stat(count)), stat = "bin")
