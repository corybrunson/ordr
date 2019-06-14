# Compare relative frequences of hair and eye colors among men versus women
HairEyeColor[, , "Male"] %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> male_haireye_ca
HairEyeColor[, , "Female"] %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> female_haireye_ca
ca_biplot <- function(ord) {
  ggbiplot(ord, aes(label = .name)) +
    geom_u_point(aes(size = .mass), color = "saddlebrown") +
    geom_v_point(aes(size = .mass), color = "seagreen4") +
    geom_u_text_repel(color = "saddlebrown") +
    geom_v_text_repel(color = "seagreen4") +
    guides(size = "none")
}
plot(gridExtra::arrangeGrob(grobs = list(
  ca_biplot(male_haireye_ca),
  ca_biplot(female_haireye_ca)
), ncol = 2))
# negation, permutation, and rotation
male_haireye_ca %>%
  negate_to(female_haireye_ca, "u") %>%
  get_alignment()
male_haireye_ca %>%
  permute_to(female_haireye_ca, "u") %>%
  get_alignment()
male_haireye_ca %>%
  rotate_to(female_haireye_ca, "u") %>%
  get_alignment()
plot(gridExtra::arrangeGrob(grobs = list(
  ca_biplot(rotate_to(male_haireye_ca, female_haireye_ca, "u")),
  ca_biplot(female_haireye_ca)
), ncol = 2))