# Compare PCA to LRA on the Freestone primary class composition data
# following Baxter & Freestone (2006)
# (do not exclude compositional outliers)
data(glass)
glass %>%
  dplyr::filter(Site != "Banias") %>%
  dplyr::mutate(Type = dplyr::case_when(
    Site == "Dor" ~ "Levantine I",
    Site == "Apollonia" ~ "Levantine I",
    Site == "Bet Eli'ezer" ~ "Levantine II"
  )) %>%
  print() -> levantine_glass
# scaled principal components analysis
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO, FeO, MgO) %>%
  princomp(cor = TRUE) %>%
  as_tbl_ord() %>%
  augment() %>%
  cbind_rows(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> glass_pca
ggbiplot(glass_pca) +
  geom_rows_point(aes(shape = Site, color = Type))
# completely compositional log-ratio analysis
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO, FeO, MgO) %>%
  lra(compositional = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia("rows") %>%
  cbind_rows(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> glass_lra
ggbiplot(glass_lra, sec.axes = "cols", scale.factor = .05) +
  geom_rows_point(aes(shape = Site, color = Type)) +
  geom_cols_vector() +
  geom_cols_text(aes(label = .name), hjust = "outward", vjust = "outward") +
  expand_limits(x = c(-.2, .2))
# completely compositional log-ratio analysis with FeO and MgO excluded
levantine_glass %>%
  dplyr::select(SiO2, Al2O3, CaO) %>%
  lra(compositional = TRUE) %>%
  as_tbl_ord() %>%
  confer_inertia("rows") %>%
  cbind_rows(dplyr::select(levantine_glass, Site, Type)) %>%
  print() -> glass_lra
ggbiplot(glass_lra, sec.axes = "cols", scale.factor = .05) +
  geom_rows_point(aes(shape = Site, color = Type)) +
  geom_cols_vector() +
  geom_cols_text(aes(label = .name), hjust = "outward", vjust = "outward") +
  expand_limits(x = c(NA, .2))
