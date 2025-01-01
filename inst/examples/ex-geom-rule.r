# stack loss gradient
stackloss |> 
  lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.) |> 
  coef() |> 
  as.list() |> as.data.frame() |> 
  subset(select = c(Air.Flow, Water.Temp, Acid.Conc.)) ->
  coef_data
# gradient rule with respect to two predictors
stackloss_centered <- scale(stackloss, scale = FALSE)
stackloss_centered |> 
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  stat_rule(
    data = coef_data,
    referent = stackloss_centered,
    fun.offset = \(x) minabspp(x, p = .5)
  )
# NB: `geom_rule(stat = "rule")` would fail to pass positional aesthetics.
