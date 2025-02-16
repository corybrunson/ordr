# cartesian coordinates
us_center <- sapply(state.center, \(x) (min(x) + max(x)) / 2)
state_center <- cbind(
  state = state.abb,
  sweep(as.data.frame(state.center), 2, us_center, "-")
)
ggplot(state_center, aes(x, y, label = state)) +
  coord_equal() +
  geom_vector() +
  geom_text_radiate()
# using arrowhead to encode a variable
state_center$coast <- ifelse(
  state.region == "North Central",
  NA,
  state.region == "West"
)
ggplot(state_center, aes(x, y, label = state)) +
  coord_equal() +
  geom_vector(aes(head = coast)) +
  geom_text_radiate()

# multidimensional scaling of covariances
ability.cov$cov |> 
  cov2cor() |>
  eigen() |> getElement("vectors") |> 
  as.data.frame() |> 
  transform(test = rownames(ability.cov$cov)) ->
  ability_cor_eigen
ability_cor_eigen |> 
  ggplot(aes(-V1, V2, label = test)) +
  coord_square() + theme_void() +
  geom_vector(check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = .2)) +
  ggtitle("Ability and intelligence test covariances")
# multidimensional scaling of correlations
ability.cov$cov |> 
  eigen() |> getElement("vectors") |> 
  as.data.frame() |> 
  transform(test = rownames(ability.cov$cov)) ->
  ability_cor_eigen
ability_cor_eigen |> 
  ggplot(aes(-V1, -V2, label = test)) +
  coord_square() + theme_void() +
  geom_vector(check_overlap = TRUE) +
  geom_unit_circle() +
  expand_limits(x = c(-1, 1), y = c(-1, 1)) +
  ggtitle("Ability and intelligence test covariances")
