# two-group karate club data
data(karate, package = "igraphdata")
# force-directed layout of graph
igraph::plot.igraph(
  karate,
  vertex.label.family = "sans",
  vertex.label.color = "black",
  vertex.label.cex = .75
)
# graph Laplacian eigenvectors for spectral partitioning:
# complete eigendecomposition, augmenting vertex data
karate %>%
  igraph::as_adjacency_matrix(sparse = FALSE) %>%
  eigen_ord() %>% as_tbl_ord() %>%
  augment_ord() %>%
  mutate_rows(
    faction = igraph::vertex_attr(karate, "Faction"),
    name = igraph::vertex_attr(karate, "name"),
    label = igraph::vertex_attr(karate, "label")
  ) %>%
  print() -> karate_eigen
# summarize ordination
glance(karate_eigen)
# recover matrices of row and column coordinates
get_rows(karate_eigen)[seq(8L), seq(5L)]
get_cols(karate_eigen)[seq(8L), seq(5L)]
# summarize eigenvectors
tidy(karate_eigen)
# validate eigencentrality calculations against igraph implementation
tibble::tibble(
  eigen = karate_eigen$vectors[, 1L] / max(karate_eigen$vectors[, 1L]),
  igraph = igraph::eigen_centrality(karate)$vector
) %>%
  ggplot(aes(x = eigen, y = igraph)) +
  coord_equal() +
  geom_abline(slope = 1, color = "#777777") +
  geom_point()
# first and second eigenvectors for centrality and connectivity / partitioning
karate_eigen %>%
  confer_inertia(1) %>%
  ggbiplot(aes(x = 2, y = 1)) +
  theme_biplot() +
  geom_rows_vector(aes(color = as.factor(faction))) +
  geom_rows_text_radiate(aes(label = ifelse(grepl("H|A", label), label, NA))) +
  guides(color = FALSE) +
  labs(x = "algebraic connectivity", y = "eigencentrality") +
  ggtitle(
    "First (centrality) and second (connectivity) eigenvectors",
    "Colors distinguish factions around John A. and Mr. Hi"
  )
# second and third eigenvectors
karate_eigen %>%
  confer_inertia(1) %>%
  ggbiplot(aes(x = 2, y = 3)) +
  theme_biplot() +
  geom_rows_vector(aes(color = EV1)) +
  geom_rows_text_radiate(stat = "chull", aes(label = label)) +
  labs(x = "algebraic connectivity", y = "loading onto third eigenvector") +
  ggtitle("Second (connectivity) and third eigencentralities")
