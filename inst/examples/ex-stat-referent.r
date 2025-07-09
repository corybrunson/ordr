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
# use the gradient as a reference (to no effect in this basic ggproto)
ggplot(subcars, aes(x = hp00, y = wt)) +
  coord_equal() +
  geom_point() +
  stat_referent(referent = grad)
ggplot(subcars, aes(x = hp00, y = wt)) +
  coord_equal() +
  stat_referent(geom = "point", referent = grad)
