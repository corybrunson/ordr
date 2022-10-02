# table of admissions and rejections from UC Berkeley
class(UCBAdmissions)
ucb_admissions <- as.data.frame(UCBAdmissions)
ucb_admissions <-
  ucb_admissions[rep(seq(nrow(ucb_admissions)), ucb_admissions$Freq), -4L]
head(ucb_admissions)
# perform multiple correspondence analysis
ucb_admissions %>%
  MASS::mca() %>%
  as_tbl_ord() %>%
  # augment profiles with names, masses, distances, and inertias
  augment_ord() %>%
  print() -> admissions_mca

# recover row and column coordinates and row weights
head(get_rows(admissions_mca, elements = "score"))
get_cols(admissions_mca)
head(get_rows(admissions_mca))

# column-standard biplot of factor levels
admissions_mca %>%
  ggbiplot() +
  theme_bw() + theme_biplot() +
  geom_origin() +
  #geom_rows_point(stat = "unique") +
  geom_cols_point(aes(color = factor, shape = factor)) +
  geom_cols_text_repel(aes(label = level, color = factor),
                       show.legend = FALSE) +
  scale_color_brewer(palette = "Dark2") +
  scale_size_area(guide = "none") +
  labs(color = "Factor level", shape = "Factor level")
