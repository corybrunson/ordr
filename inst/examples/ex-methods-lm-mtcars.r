# Motor Trend design and performance data
head(mtcars)
# regression analysis of performance measures on design specifications
mtcars_centered <- scale(mtcars, scale = FALSE)
mtcars_centered %>%
  as.data.frame() %>%
  lm(formula = mpg ~ wt + cyl) %>%
  print() -> mtcars_lm

# wrap as a 'tbl_ord' object
(mtcars_lm_ord <- as_tbl_ord(mtcars_lm))
# augment everything with names, predictors with observation stats
augment_ord(mtcars_lm_ord)
# calculate influences as the squares of weighted residuals
mutate_rows(augment_ord(mtcars_lm_ord), influence = wt.res^2)

# regression biplot with performance isolines
mtcars_lm_ord %>%
  augment_ord() %>%
  mutate_cols(center = attr(mtcars_centered, "scaled:center")[name]) %>%
  mutate_rows(influence = wt.res^2) %T>% print() %>%
  ggbiplot(aes(x = wt, y = cyl, intercept = `(Intercept)`)) +
  #theme_biplot() +
  geom_origin(marker = "circle", radius = unit(0.02, "snpc")) +
  geom_rows_point(aes(color = influence)) +
  geom_cols_vector() +
  geom_cols_isoline(aes(center = center), by = .5, hjust = -.1) +
  ggtitle(
    "Weight isolines with data colored by importance",
    "Regressing gas mileage onto weight and number of cylinders"
  )
