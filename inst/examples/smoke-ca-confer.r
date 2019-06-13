# Row- and column-principal biplots for CA of artificial smoking frequency data
data(smoke, package = "ca")
smoke %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> smoke_ca
smoke_ca %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "green") +
  geom_u_text_repel(color = "blue") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")
smoke_ca %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "green") +
  geom_u_text_repel(color = "blue") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")
