# correspondence analysis of combined hair and eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
haireye %>%
  ordinate(MASS::corresp, cols = everything(), nf = 3L) %>%
  confer_inertia("symmetric") %>%
  print() -> hec_ca

# facet the first three correspondence coordinates
hec_ca %>%
  ggbiplot() +
  facet_ord(num_coord = 3) +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with fixed scales and space"
  )

# free space (by default frees the scales)
hec_ca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, space = "free") +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with free scales and space"
  )

# free scales, fixed space: panel scales determined by coordinates (discouraged)
hec_ca %>%
  ggbiplot() +
  facet_ord(num_coord = 3, scales = "free", space = "fixed") +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with free scales and fixed space"
  )
