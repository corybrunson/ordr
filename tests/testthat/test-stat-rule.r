t <- asin(sqrt(seq(0, 4)) / 2)
unit_data <- data.frame(
  x = cos(t),
  y = sin(t)
)
s <- runif(12, 0, 2*pi)
cloud_data <- data.frame(
  x = 2 * cos(s),
  y = 2 * sin(s)
)
axis_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  geom_axis()
rule_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.min = NULL, fun.max = NULL, fun.offset = NULL
  )
limit_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.min = minpp, fun.max = maxpp, fun.offset = NULL
  )
# FIXME
offset_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.min = NULL, fun.max = NULL, fun.offset = minabspp
  )
