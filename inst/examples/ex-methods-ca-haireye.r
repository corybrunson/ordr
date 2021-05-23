# table of hair and eye color data collapsed by sex
class(HairEyeColor)
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
print(haireye)
# use correspondence analysis to construct row and column profiles
haireye %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> haireye_ca
# summarize ordination
glance(haireye_ca)
# recover row and column profiles
get_rows(haireye_ca)
get_cols(haireye_ca)
# augment profiles with names, masses, distances, and inertias
augment_ord(haireye_ca)
# summarize artificial coordinates
tidy(haireye_ca)
# scree plot of inertia
tidy(haireye_ca) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# fortification adds all above columns
fortify(haireye_ca)
# symmetric biplot
haireye_ca %>%
  confer_inertia("symmetric") %>%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() + theme_biplot() +
  geom_origin() +
  geom_rows_point(aes(size = .inertia)) +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point(aes(size = .inertia)) +
  geom_cols_text_repel(show.legend = FALSE) +
  scale_size_area() +
  labs(color = "Dimension", shape = "Dimension", size = "Inertia")
# symmetric map biplot (both dimensions in principal coordinates)
haireye_ca %>%
  confer_inertia(c(1, 1)) %>%
  ggbiplot(aes(label = .name, color = .matrix, shape = .matrix)) +
  scale_color_manual(values = c("navyblue", "brown")) +
  scale_shape_manual(values = c(17L, 16L)) +
  theme_bw() + theme_biplot() +
  geom_origin() +
  geom_rows_point(aes(size = .inertia)) +
  geom_rows_text_repel(show.legend = FALSE) +
  geom_cols_point(aes(size = .inertia)) +
  geom_cols_text_repel(show.legend = FALSE) +
  scale_size_area() +
  labs(color = "Dimension", shape = "Dimension", size = "Inertia")
