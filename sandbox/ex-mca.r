
# Exhibit 9.4 (experimental)
lapply(1:8, function(i) {
  q <- LETTERS[i]
  d <- issp_women[, str_detect(colnames(issp_women), q)]
  c <- fortify(confer_inertia(as_tbl_ord(ca::ca(d)), "rowprincipal"))
  r <- filter(c, str_detect(.name, "^e"))
  mutate(r, question = q)
}) %>%
  bind_rows() %>%
  print() -> d
confer_inertia(b, c(1, 1)) %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "darkgreen") +
  geom_u_text_repel(color = "darkgreen") +
  geom_u_point(data = filter(d, .name == "e5"), color = "brown", shape = 15)
