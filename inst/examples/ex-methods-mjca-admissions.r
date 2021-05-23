# data on admissions and rejections from UC Berkeley
class(UCBAdmissions)
head(as.data.frame(UCBAdmissions))
# perform multiple correspondence analysis
UCBAdmissions %>%
  ca::mjca() %>%
  as_tbl_ord() %>%
  print() -> admissions_mca
# summarize ordination
glance(admissions_mca)
# recover row and column profiles
head(get_rows(admissions_mca))
get_cols(admissions_mca)
# augment profiles with names, masses, distances, and inertias
augment_ord(admissions_mca)
# summarize artificial coordinates
tidy(admissions_mca)
# scree plot of inertia
tidy(admissions_mca) %>%
  ggplot(aes(x = .name, y = .inertia)) +
  theme_bw() +
  geom_col() +
  labs(x = "", y = "Inertia")
# scree plot of proportion of variance (inertia)
tidy(admissions_mca) %>%
  ggplot(aes(x = .name, y = .prop_var)) +
  theme_bw() +
  scale_y_continuous(labels = scales::percent) +
  geom_col() +
  labs(x = "", y = "Proportion of inertia")
# fortification adds all above columns
fortify(admissions_mca)
# column-standard biplot of factor levels
admissions_mca %>%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_origin() +
  #geom_rows_point(stat = "unique") +
  geom_cols_point(aes(color = .factor, shape = .factor)) +
  geom_cols_text_repel(aes(label = .level, color = .factor),
                       show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Factor level", shape = "Factor level")
# column-principal biplot of factor levels
admissions_mca %>%
  confer_inertia("colprincipal") %>%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_origin() +
  #geom_rows_point(stat = "unique") +
  geom_cols_point(aes(color = .factor, shape = .factor)) +
  geom_cols_text_repel(aes(label = .level, color = .factor),
                       show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  labs(color = "Factor level", shape = "Factor level")
