# stack loss gradient
stackloss |> 
  lm(formula = stack.loss ~ Air.Flow + Water.Temp + Acid.Conc.) |> 
  coef() |> 
  as.list() |> as.data.frame() |> 
  subset(select = c(Air.Flow, Water.Temp, Acid.Conc.)) ->
  coef_data
# gradient axis with respect to two predictors
stackloss_centered <- scale(stackloss, scale = FALSE)
stackloss_centered |> 
  ggplot(aes(x = Acid.Conc., y = Air.Flow)) +
  coord_square() + geom_origin() +
  geom_point(aes(size = stack.loss)) + scale_size_area() +
  # geom_rule(data = coef_data)
  # FIXME: To make the above work, pass unused inherited data to `referent`.
  geom_rule(
    data = coef_data,
    referent = stackloss_centered
  )
