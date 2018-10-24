pca <- prcomp(USPersonalExpenditure, center = FALSE)
pca_ord <- as_tbl_ord(pca)
pca_ord <- confer_inertia(pca_ord, c(1, 0))
ggbiplot(pca_ord, aes(label = .name)) +
  theme_bw() +
  scale_x_continuous() +
  geom_v_vector() +
  geom_u_point() +
  geom_u_text_repel()
ggbiplot(pca_ord, aes(label = .name)) +
  theme_bw() +
  scale_x_continuous(sec.axis = biplot_sec_axis(scale = 5)) +
  geom_v_vector() +
  geom_u_point() +
  geom_u_text_repel()
ggbiplot(pca_ord, aes(label = .name), sec.axes = "v") +
  theme_bw() +
  geom_v_vector() +
  geom_u_point() +
  geom_u_text_repel()
