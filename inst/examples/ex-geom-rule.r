# stack loss gradient
stackloss |> 
  lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.) |> 
  coef() |> 
  as.list() |> as.data.frame() |> 
  subset(select = c(Air.Flow, Water.Temp, Acid.Conc.)) ->
  coef_data
# gradient ruler with respect to two predictors
stackloss_centered <- scale(stackloss, scale = FALSE)
stackloss_centered |> 
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss, alpha = sign(stack.loss))) + 
  scale_size_area() + scale_alpha_binned(breaks = c(-1, 0, 1)) +
  geom_rule(
    data = coef_data,
    referent = stackloss_centered[, c("Acid.Conc.", "Air.Flow")],
    fun.offset = \(x) minabspp(x, p = .5)
  )
