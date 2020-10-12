# Confer inertia in CA between rows and columns of benthic sample data
data(benthos)
benthos %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> benthos_ca
# Reproduce Exhibit 8.3
benthos_ca %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "brown", size = 1) +
  geom_v_text(color = "darkgreen")
# Reproduce Exhibit 8.4
benthos_ca %>%
  augment(.matrix = "u") %>%
  transmute_u(mass = .mass, sqrt_mass = sqrt(.mass)) %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name), scale_u = "sqrt_mass") +
  theme_bw() +
  geom_u_vector(color = "brown", arrow = NULL) +
  geom_u_point(aes(size = mass), color = "brown", shape = 17) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  geom_u_text(
    stat = "chull",
    color = "brown", hjust = "outward", vjust = "outward"
  ) +
  geom_v_point(color = "darkgreen") +
  geom_v_text_repel(color = "darkgreen", min.segment.length = 2)
