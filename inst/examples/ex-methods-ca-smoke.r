# data frame of artificial employee smoking habits
class(ca::smoke)
print(ca::smoke)
# use correspondence analysis to construct row and column profiles
ca::smoke %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> smoke_ca
# recover row and column profiles
get_rows(smoke_ca)
get_cols(smoke_ca)
# augment profiles with names, masses, distances, and inertias
augment_ord(smoke_ca)
# summarize artificial coordinates
tidy(smoke_ca)
# fortification of artificial coordinates yields proportion of variance measure
fortify(smoke_ca, .matrix = "coord")
# scree plot of inertia
ggplot(smoke_ca, .matrix = "coord", aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
ggplot(smoke_ca, .matrix = "coord", aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_bar(stat = "identity") +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(smoke_ca)
# row-principal biplot
smoke_ca %>%
  confer_inertia("rowprincipal") %>%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() +
  geom_rows_point() +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point() +
  geom_cols_text_repel(show.legend = FALSE) +
  labs(color = "Dimension", shape = "Dimension")
# row-principal biplot
smoke_ca %>%
  confer_inertia("colprincipal") %>%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() +
  geom_rows_point() +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point() +
  geom_cols_text_repel(show.legend = FALSE) +
  labs(color = "Dimension", shape = "Dimension")
