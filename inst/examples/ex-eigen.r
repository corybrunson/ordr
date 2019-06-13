library(igraph)

data(karate, package = "igraphdata")
plot(karate, vertex.label = ifelse(
  stringr::str_detect(V(karate)$label, "H|A"), V(karate)$name, NA
))

# eigenvector centrality using *igraph*
karate_eigen_cent <- eigen_centrality(karate)

# eigendecomposition using base R
karate_eigen_ord <- karate %>%
  as_adjacency_matrix(sparse = FALSE) %>%
  eigen() %>% as_tbl_ord() %>%
  mutate_u(
    faction = vertex_attr(karate, "Faction"),
    name = vertex_attr(karate, "name"),
    label = vertex_attr(karate, "label")
  )

# verify that "first" eigencentralities agree (up to rounding error)
tibble::tibble(
  igraph = karate_eigen_cent$vector,
  eigen = karate_eigen_ord$vectors[, 1]
) %>%
  ggplot(aes(x = eigen, y = igraph)) +
  geom_point()

# biplot of first and second eigencentralities
karate_eigen_ord %>%
  ggbiplot() +
  geom_u_vector() +
  geom_u_text_radiate(aes(label = label))

# check whether "second" eigencentralities agree with conventional partition
# http://www.sfu.ca/personal/archives/richards/Pages/london98.pdf
# http://netwiki.amath.unc.edu/uploads/Publications/Ma11Paper_final.pdf
karate_eigen_ord %>%
  ggbiplot(aes(x = 2, y = 3)) +
  scale_x_continuous(expand = expand_scale(mult = .3)) +
  scale_y_continuous(expand = expand_scale(mult = .2)) +
  geom_u_vector(aes(color = as.factor(faction))) +
  geom_u_text_radiate(aes(label = ifelse(
    stringr::str_detect(label, "H|A"), name, NA
  ))) +
  guides(color = FALSE)
