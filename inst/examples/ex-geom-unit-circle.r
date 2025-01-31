iris |> 
  split(~ Species) |> 
  lapply(subset, select = -c(Species)) |> 
  lapply(scale, center = TRUE, scale = FALSE) |> 
  lapply(as.data.frame) |> 
  unsplit(iris$Species) |> 
  transform(Species = iris$Species) ->
  iris_ctr
ggplot(iris_ctr, aes(Petal.Width, Petal.Length)) +
  coord_equal() +
  facet_wrap(vars(Species)) +
  geom_unit_circle() +
  geom_point()
