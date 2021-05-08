# incomplete air quality measurements from New York
class(airquality)
head(airquality)
# single date variable
airquality %>%
  transform(Date = as.Date(paste("1973", Month, Day, sep = "-"))) %>%
  subset(select = -c(Month, Day)) ->
  air_quality
# NIPALS on air quality measures
air_quality[, seq(4L)] %>%
  nipals_ord() %>%
  as_tbl_ord() %>%
  print() -> air_nipals
# summarize ordination
glance(air_nipals)
# bind dates to observation coordinates
(air_nipals <- bind_cols_rows(air_nipals, air_quality[, 5L, drop = FALSE]))
# by default, no inertia is conferred
get_conference(air_nipals)
# recover observation and measurement standard coordinates
head(get_rows(air_nipals))
get_cols(air_nipals)
# augment measurements with names and scaling parameters
augment_ord(air_nipals)
# summarize principal components
tidy(air_nipals)
# fortification of artificial coordinates yields proportion of variance measure
fortify(air_nipals, .matrix = "coord")
# scree plot of inertia
ggplot(air_nipals, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
ggplot(air_nipals, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(air_nipals)
# row-principal biplot with monthly ellipses
air_nipals %>%
  confer_inertia("rows") %>%
  ggbiplot() +
  theme_bw() +
  geom_cols_vector(color = "#444444") +
  geom_cols_text_radiate(aes(label = .name), color = "#444444") +
  stat_rows_ellipse(aes(color = format(Date, "%b"))) +
  geom_rows_point(aes(color = format(Date, "%b")), size = 2, alpha = .5) +
  ggtitle("Row-principal PCA biplot of 1973 air quality measurements") +
  labs(color = "Month")
