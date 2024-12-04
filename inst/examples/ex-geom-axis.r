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
  geom_point(aes(size = stack.loss)) + scale_size_area() +
  geom_axis(data = coef_data)
