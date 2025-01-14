# Motor Trends base plot with factorized cylinder counts
p <- mtcars |> 
  transform(cyl = factor(cyl)) |> 
  ggplot(aes(x = wt, y = disp)) +
  theme_bw()
# basic bagplot
# FIXME: Fence defaults to dotted rather than invisible `linetype`.
p + geom_bagplot()
# group by cylinder count
# FIXME: Component aesthetic data values aren't mapped to color values.
set.seed(526822L)
p + geom_bagplot(
  fraction = 0.4, coef = 1.2,
  aes(
    fill = cyl,
    linetype = cyl,# fence_linetype = cyl,
    median_colour = cyl, outlier_colour = cyl
  )
)
# fixed custom aesthetics (bag and fence shrunk to expose outliers)
set.seed(009003L)
p + geom_bagplot(
  fraction = 0.4, coef = 1.2,
  aes(fill = cyl), shape = "plus", outlier_shape = "asterisk"
)
