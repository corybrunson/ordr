# Performance measures can be regressed on the artificial coordinates of
# ordinated vehicle specs. Because the ordination of specs ignores performance,
# these coordinates will probably not be highly predictive. The gradient of each
# performance measure along the artificial axes is visualized by projecting the
# regression coefficients onto the ordination biplot.

# scaled principal components analysis of vehicle specs
mtcars_specs_pca <- ordinate(
  mtcars, cols = c(cyl, disp, hp, drat, wt, vs, carb),
  model = ~ princomp(., cor = TRUE)
)
# data frame of vehicle performance measures
mtcars %>%
  subset(select = c(mpg, qsec)) %>%
  as.matrix() %>%
  print() -> mtcars_perf
# regress performance measures on principal components
lm(mtcars_perf ~ get_rows(mtcars_specs_pca)) %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> mtcars_pca_lm
# regression biplot
ggbiplot(mtcars_specs_pca, aes(label = name),
         sec.axes = "rows", scale.factor = .5) +
  theme_minimal() +
  geom_rows_text(size = 3) +
  geom_cols_vector(data = mtcars_pca_lm) +
  geom_cols_text_radiate(data = mtcars_pca_lm) +
  expand_limits(x = c(-2.5, 2))

# multidimensional scaling based on a scaled cosine distance of vehicle specs
cosine_dist <- function(x) {
  x <- as.matrix(x)
  num <- x %*% t(x)
  denom_rt <- as.matrix(rowSums(x^2))
  denom <- sqrt(denom_rt %*% t(denom_rt))
  as.dist(1 - num / denom)
}
mtcars %>%
  subset(select = c(cyl, disp, hp, drat, wt, vs, carb)) %>%
  scale() %>%
  cosine_dist() %>%
  cmdscale() %>%
  as.data.frame() ->
  mtcars_specs_cmds
# names must be consistent with `cmdscale_ord()` below
names(mtcars_specs_cmds) <- c("PCo1", "PCo2")
# regress performance measures on principal coordinates
lm(mtcars_perf ~ as.matrix(mtcars_specs_cmds)) %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> mtcars_cmds_lm
# multidimensional scaling using `cmdscale_ord()`
mtcars %>%
  subset(select = c(cyl, disp, hp, drat, wt, vs, carb)) %>%
  scale() %>%
  cosine_dist() %>%
  cmdscale_ord() %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  print() -> mtcars_specs_cmds_ord
# regression biplot
ggbiplot(mtcars_specs_cmds_ord, aes(label = name),
         sec.axes = "rows", scale.factor = 3) +
  theme_minimal() +
  geom_rows_text(size = 3) +
  geom_cols_vector(data = mtcars_cmds_lm) +
  geom_cols_text_radiate(data = mtcars_cmds_lm) +
  expand_limits(x = c(-2.25, 1.25), y = c(-2, 1.5))
