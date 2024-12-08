# axes at popular trig angles
t <- asin(sqrt(seq(0, 4)) / 2)
unit_data <- data.frame(
  x = cos(t),
  y = sin(t)
)
# circular point cloud with gap in quadrant 4
set.seed(76393L)
s <- runif(24, 0, 3/2*pi)
cloud_data <- data.frame(
  x = 2 * cos(s),
  y = 2 * sin(s)
)

axis_plot0 <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  geom_axis()
axis_layer0 <- layer_data(axis_plot0)
axis_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.min = NULL, fun.max = NULL, fun.offset = NULL
  )
axis_layer <- layer_data(axis_plot)

test_that("`stat_rule()` reflects `geom_axis()` in absence of functions", {
  layer_names0 <- setdiff(names(axis_layer0), "group")
  expect(
    all(mapply(
      all.equal,
      axis_layer0[, layer_names0],
      axis_layer[order(axis_layer$axis), layer_names0]
    )),
    failure_message = "Built data frames disagree."
  )
})

limits_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.offset = NULL
  )
limits_layer <- layer_data(limits_plot)
offset_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(
    referent = cloud_data,
    fun.min = NULL, fun.max = NULL
  )
offset_layer <- layer_data(offset_plot)
rule_plot <- 
  ggplot(unit_data, aes(x, y)) +
  coord_equal() +
  stat_rule(referent = cloud_data)
rule_layer <- layer_data(rule_plot)

test_that("limit coordinates lie in quadrants 1 and 3", {
  expect_true(all(as.matrix(limits_layer[, c("xmin", "ymin")]) <= 0))
  expect_true(all(as.matrix(limits_layer[, c("xmax", "ymax")]) >= 0))
  
  rule_diff <- 
    as.matrix(rule_layer[, c("xmax", "ymax")]) -
    as.matrix(rule_layer[, c("xmin", "ymin")])
  expect_true(all(rule_diff >= 0))
})

test_that("oblique offset coordinates lie in quadrant 4", {
  offset_oblique <- offset_layer$angle %% (pi/2) != 0
  expect_true(all(offset_layer$xend[offset_oblique] >= 0))
  
  rule_oblique <- rule_layer$angle %% (pi/2) != 0
  expect_true(all(rule_layer$xend[rule_oblique] >= 0))
})
