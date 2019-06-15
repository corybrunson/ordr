# Graph Laplacian eigenvectors for spectral partitioning
data(karate, package = "igraphdata")
igraph::plot.igraph(
  karate,
  vertex.label.family = "sans",
  vertex.label.color = "black",
  vertex.label.cex = .75
)
# first eigenvector (centrality)
karate_eigen_cent <- igraph::eigen_centrality(karate)
# eigendecomposition
karate %>%
  igraph::as_adjacency_matrix(sparse = FALSE) %>%
  eigen_ord() %>% as_tbl_ord() %>%
  mutate_u(
    faction = igraph::vertex_attr(karate, "Faction"),
    name = igraph::vertex_attr(karate, "name"),
    label = igraph::vertex_attr(karate, "label")
  ) %>%
  print() -> karate_eigen
# corroborate eigencentralities
tibble::tibble(
  igraph = karate_eigen_cent$vector,
  eigen = karate_eigen$vectors[, 1] / max(karate_eigen$vectors[, 1])
) %>%
  ggplot(aes(x = eigen, y = igraph)) +
  coord_equal() +
  geom_point()
# first and second eigenvectors for centrality and connectivity / partitioning
karate_eigen %>%
  ggbiplot(aes(x = 2, y = 1)) +
  geom_u_vector(aes(color = as.factor(faction))) +
  geom_u_text_radiate(aes(label = ifelse(grepl("H|A", label), label, NA))) +
  guides(color = FALSE) +
  labs(x = "algebraic connectivity", y = "eigencentrality") +
  ggtitle(
    "First (centrality) and second (connectivity) eigenvectors",
    "Colors distinguish factions around John A. and Mr. Hi"
  )
# second and third eigenvectors
karate_eigen %>%
  ggbiplot(aes(x = 2, y = 3)) +
  scale_x_continuous(expand = expand_scale(mult = .3)) +
  scale_y_continuous(expand = expand_scale(mult = .2)) +
  geom_u_vector(aes(color = EV1)) +
  geom_u_text_radiate(stat = "chull", aes(label = label)) +
  labs(x = "algebraic connectivity", y = "loading onto third eigenvector") +
  ggtitle(
    "Second (horizontal) and third (vertical) eigencentralities",
    "Color increases in value with eigencentrality, labels along convex hull"
  )
