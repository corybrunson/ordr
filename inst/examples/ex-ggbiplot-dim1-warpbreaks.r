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
warp_ca |>
  ggbiplot(aes(x = 1)) +
  geom_origin() +
  geom_rows_bar(color = "blue") +
  geom_cols_bar(aes(y = -after_stat(count)), color = "red")
# TODO: Initialize plot with no vertical axis;
# check that it is created when a layer expects it, e.g. `stat = "bin"`.
warp_ca |> 
  ggbiplot(aes(label = name, color = .matrix)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_rows_pin(ymax = .5) +
  geom_cols_pin(ymax = -.5)

iris_pca <- ordinate(iris, cols = 1:4, model = ~ prcomp(., scale. = TRUE))
# TODO: Have secondary axis specification determine ordinate reversal.
iris_pca |> 
  ggbiplot(aes(x = 1), sec.axes = "cols", scale.factor = 10) +
  geom_vline(xintercept = 0) +
  geom_rows_density(aes(color = Species)) +
  geom_cols_bar(aes(y = -after_stat(count)), stat = "bin")
