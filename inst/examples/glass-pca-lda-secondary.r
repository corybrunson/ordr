# Compare PCA to LRA on the Freestone primary class composition data
# following Baxter & Freestone (2006)
# (do not exclude compositional outliers)
levantine_glass <- glass %>%
  dplyr::filter(Site != "Banias") %>%
  dplyr::mutate(Type = dplyr::case_when(
    Site == "Dor" ~ "Levantine I",
    Site == "Apollonia" ~ "Levantine I",
    Site == "Bet Eli'ezer" ~ "Levantine II"
  ))
# scaled principal components analysis
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO, FeO, MgO) %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  bind_cols_u(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> pca_glass
ggbiplot(pca_glass) +
  geom_u_point(aes(shape = Site, color = Type))
# completely compositional log-ratio analysis
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO, FeO, MgO) %>%
  lra(compositional = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia("rows") %>%
  bind_cols_u(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> lda_glass
ggbiplot(lda_glass, sec.axes = "v", scale.factor = .05) +
  geom_u_point(aes(shape = Site, color = Type)) +
  geom_v_vector() +
  geom_v_text(aes(label = .name), hjust = "outward", vjust = "outward")
# completely compositional log-ratio analysis with FeO and MgO excluded
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO) %>%
  lra(compositional = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia("rows") %>%
  bind_cols_u(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> lda_glass
ggbiplot(lda_glass, sec.axes = "v", scale.factor = .05) +
  geom_u_point(aes(shape = Site, color = Type)) +
  geom_v_vector() +
  geom_v_text(aes(label = .name), hjust = "outward", vjust = "outward")
