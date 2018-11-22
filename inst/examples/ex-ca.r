
data(smoke, package = "ca")

(m <- ca::ca(smoke))

plot(m, map = "rowprincipal", col = c("green", "brown"))

(b <- as_tbl_ord(m))
(d <- fortify(b))

# confer singular values to reproduce the row-principal biplot
set.seed(999291)
b <- confer_inertia(b, "rowprincipal")
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "green") +
  geom_u_text_repel(color = "blue") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")

# the column-principal biplot, for comparison
set.seed(861282)
b <- confer_inertia(b, "colprincipal")
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "green") +
  geom_u_text_repel(color = "blue") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")

data(benthos)

(m <- ca::ca(benthos))

plot(
  m, map = "colprincipal",
  col = c("brown", "darkgreen"),
  labels = c(0, 1), col.lab = c("brown", "darkgreen")
)

(b <- as_tbl_ord(m))
(d <- fortify(b))

# reproduce Exhibit 8.3
b %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "brown", size = 1) +
  geom_v_text(color = "darkgreen")

# reproduce Exhibit 8.4
set.seed(668286)
b %>%
  augment(.matrix = "u") %>%
  transmute_u(mass = .mass, sqrt_mass = sqrt(.mass)) %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name), scale_u = "sqrt_mass") +
  theme_bw() +
  geom_u_vector(color = "brown", arrow = NULL) +
  geom_u_point(aes(size = mass), color = "brown", shape = 17) +
  scale_size_continuous(range = c(1, 4), guide = "none") +
  geom_u_text(stat = "chull", color = "brown", nudge_x = .075, nudge_y = .05) +
  geom_v_point(color = "darkgreen") +
  geom_v_text_repel(color = "darkgreen", min.segment.length = 2)

# reproduce Exhibits 9.2, 9.3, 9.4, 9.6 in Greenacre (2010)

data(issp_women)

(m <- ca::ca(issp_women))
(b <- augment(as_tbl_ord(m)))

# Exhibit 9.2
b %>%
  confer_inertia(c(1, 1)) %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "darkgreen") +
  geom_u_text_repel(color = "darkgreen") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")

# Exhibit 9.3
b %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "darkgreen") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")
