# multidimensional scaling of covariances
ability.cov$cov %>%
  cov2cor() %>%
  eigen() %>% getElement("vectors") %>%
  as.data.frame() %>%
  transform(test = rownames(ability.cov$cov)) ->
  ability_cor_eigen
ability_cor_eigen %>%
  ggplot(aes(-V1, V2, label = test)) +
  coord_square() + theme_void() +
  geom_vector(check_overlap = TRUE) +
  scale_y_continuous(expand = expansion(mult = .2)) +
  ggtitle("Ability and intelligence test covariances")
# multidimensional scaling of correlations
ability.cov$cov %>%
  eigen() %>% getElement("vectors") %>%
  as.data.frame() %>%
  transform(test = rownames(ability.cov$cov)) ->
  ability_cor_eigen
ability_cor_eigen %>%
  ggplot(aes(-V1, -V2, label = test)) +
  coord_square() + theme_void() +
  geom_vector(check_overlap = TRUE) +
  geom_unit_circle() +
  expand_limits(x = c(-1, 1), y = c(-1, 1)) +
  ggtitle("Ability and intelligence test covariances")
