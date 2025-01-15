# Motor Trends base plot with factorized cylinder counts
p <- mtcars |> 
  transform(cyl = factor(cyl)) |> 
  ggplot(aes(x = wt, y = disp)) +
  theme_bw()
# basic bagplot
# FIXME: Fence defaults to dotted rather than invisible `linetype`.
p + geom_bagplot()
# group by cylinder count using normally unmapped aesthetics
set.seed(526822L)
p + geom_bagplot(
  fraction = 0.4, coef = 1.2,
  aes(fill = cyl, linetype = cyl),
  fence.linetype = sync(), outlier.shape = "asterisk", outlier.colour = sync()
)
