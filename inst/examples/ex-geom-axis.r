# stack loss gradient
stackloss %>%
  lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.) %>%
  coef() %>%
  as.list() %>% as.data.frame() %>%
  subset(select = c(Air.Flow, Water.Temp, Acid.Conc.)) ->
  coef_data
# gradient axis with respect to two predictors
scale(stackloss, scale = FALSE) %>%
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  geom_axis(data = coef_data)
# unlimited axes with window forcing
stackloss_centered <- scale(stackloss, scale = FALSE)
stackloss_centered %>%
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  stat_rule(
    geom = "axis", data = coef_data,
    referent = stackloss_centered,
    fun.lower = function(x) minpp(x, p = 1), fun.upper = \(x) maxpp(x, p = 1),
    fun.offset = function(x) minabspp(x, p = 1)
  )
# NB: `geom_axis(stat = "rule")` would fail to pass positional aesthetics.

# eigen-decomposition of covariance matrix
ability.cov$cov %>%
  cov2cor() %>%
  eigen() %>%getElement("vectors") %>%
  as.data.frame() %>%
  transform(test = rownames(ability.cov$cov)) ->
  ability_cor_eigen
# test axes in best-approximation space
ability_cor_eigen %>%
  transform(E3 = ifelse(V3 > 0, "rise", "fall")) %>%
  ggplot(aes(V1, V2, color = E3)) +
  coord_square() +
  geom_axis(aes(label = test), text.color = "black", text.alpha = .5) +
  expand_limits(x = c(-1, 1), y = c(-1, 1))
