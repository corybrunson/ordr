# data frame of Anderson iris species measurements
class(iris)
head(iris)
# compute 3-means clustering on scaled iris measurements
set.seed(5601L)
iris %>%
  subset(select = -Species) %>%
  scale() %>%
  kmeans(centers = 3) %>%
  print() -> iris_km

# visualize clusters using PCA
iris %>%
  subset(select = -Species) %>%
  prcomp() %>%
  as_tbl_ord() %>%
  mutate_rows(cluster = iris_km$cluster) %>%
  ggbiplot() +
  geom_rows_point(aes(color = factor(as.character(as.integer(cluster)),
                                     levels = as.character(seq(3L))))) +
  scale_color_brewer(type = "qual", name = "cluster")

# wrap as a 'tbl_ord' object
(iris_km_ord <- as_tbl_ord(iris_km))

# augment everything with names, observations with cluster assignment
(iris_km_ord <- augment_ord(iris_km_ord))

# summarize clusters with standard deviation
iris_km_ord %>%
  tidy() %>%
  transform(sdev = sqrt(withinss / size))

# discriminate between clusters 2 and 3
iris_km_ord %>%
  ggbiplot(aes(x = `2`, y = `3`), color = factor(.cluster)) +
  geom_jitter(stat = "rows", aes(shape = cluster), width = .2, height = .2) +
  geom_cols_axis(aes(color = `1`, label = name),
                 text_size = 2, text_dodge = .1,
                 label_size = 3, label_alpha = .5) +
  scale_x_continuous(expand = expansion(mult = .8)) +
  scale_y_continuous(expand = expansion(mult = .5)) +
  ggtitle(
    "Measurement loadings onto clusters 2 and 3",
    "Color indicates loadings onto cluster 1"
  )
