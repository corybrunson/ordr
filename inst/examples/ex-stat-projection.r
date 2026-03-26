# simplify the Motor Trends data to two predictors legible at aspect ratio 1
mtcars %>%
  transform(hp00 = hp/100) %>%
  subset(select = c(mpg, hp00, wt)) ->
  subcars
# compute the gradient of `mpg` against these two predictors
lm(mpg ~ hp00 + wt, subcars) %>%
  coefficients() %>%
  as.list() %>% as.data.frame() ->
  grad
# project the data onto the gradient axis (with a reversed gradient vector)
ggplot(subcars, aes(x = hp00, y = wt)) +
  coord_equal() +
  geom_point(shape = "circle open") +
  geom_vector(data = -grad) +
  stat_projection(referent = grad)
