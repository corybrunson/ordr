# Compare relative frequences of hair and eye colors among men versus women
data(HairEyeColor)
HairEyeColor[, , "Male"] %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> male_haireye_ca
HairEyeColor[, , "Female"] %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> female_haireye_ca
# side-by-side biplots
ca_biplot <- function(ord) {
  ggbiplot(ord, aes(label = .name, color = .matrix)) +
    scale_color_manual(values = c("seagreen4", "saddlebrown")) +
    geom_rows_point(aes(size = .mass)) +
    geom_cols_point(aes(size = .mass)) +
    geom_rows_text_repel() +
    geom_cols_text_repel() +
    guides(color = "none", size = "none")
}
plot(gridExtra::arrangeGrob(grobs = list(
  ca_biplot(female_haireye_ca),
  ca_biplot(male_haireye_ca)
), ncol = 2))
# faceted biplots
dplyr::bind_rows(
  dplyr::mutate(fortify(male_haireye_ca), sex = "male"),
  dplyr::mutate(fortify(female_haireye_ca), sex = "female")
) %>%
  dplyr::mutate(feature = ifelse(.matrix == "rows", "Hair", "Eye")) %>%
  ggplot(aes(x = Dim1, y = Dim2, label = .name, color = feature)) +
  facet_wrap(~ sex) +
  scale_color_manual(values = c("seagreen4", "saddlebrown")) +
  coord_equal() +
  geom_point(aes(size = .mass)) +
  geom_text_repel() +
  guides(size = "none")
