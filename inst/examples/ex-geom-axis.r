# stack loss gradient
stackloss |> 
  lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.) |> 
  coef() |> 
  as.list() |> as.data.frame() |> 
  subset(select = c(Air.Flow, Water.Temp, Acid.Conc.)) ->
  coef_data
# gradient axis with respect to two predictors
scale(stackloss, scale = FALSE) |> 
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  geom_axis(data = coef_data)
# unlimited axes with window forcing
stackloss_centered <- scale(stackloss, scale = FALSE)
stackloss_centered |> 
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  stat_rule(
    geom = "axis", data = coef_data,
    referent = stackloss_centered,
    fun.lower = \(x) minpp(x, p = 1), fun.upper = \(x) maxpp(x, p = 1),
    fun.offset = \(x) minabspp(x, p = 1)
  )
# NB: `geom_axis(stat = "rule")` would fail to pass positional aesthetics.
