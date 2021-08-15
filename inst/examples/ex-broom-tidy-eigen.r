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
  eigen() -> karate_eigen
karate_eigen %>%
  tidy() %>%
  tidyr::pivot_wider(
    id_cols = row, names_from = EV, values_from = value,
    names_prefix = "EV"
  ) %>%
  mutate(
    faction = igraph::vertex_attr(karate, "Faction"),
    name = igraph::vertex_attr(karate, "name"),
    label = igraph::vertex_attr(karate, "label")
  ) %>%
  print() -> karate_eigen_tidy
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
karate_eigen_tidy %>%
  ggplot(aes(x = EV2, y = EV1)) +
  theme_bw() +
  geom_vector(aes(color = as.factor(faction))) +
  geom_text_radiate(aes(label = ifelse(grepl("H|A", label), label, NA))) +
  guides(color = "none") +
  labs(x = "algebraic connectivity", y = "eigencentrality") +
  ggtitle(
    "First (centrality) and second (connectivity) eigenvectors",
    "Colors distinguish factions around John A. and Mr. Hi"
  )
# second and third eigenvectors
karate_eigen_tidy %>%
  ggplot(aes(x = EV2, y = EV3)) +
  theme_bw() +
  geom_vector(aes(color = EV1)) +
  geom_text_radiate(stat = "chull", aes(label = label)) +
  labs(x = "algebraic connectivity", y = "loading onto third eigenvector") +
  ggtitle("Second (connectivity) and third eigencentralities")
