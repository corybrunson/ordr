# correspondence analysis of combined hair and eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
haireye %>%
  ordinate(MASS::corresp, cols = everything(), nf = 3L) %>%
  confer_inertia("symmetric") %>%
  print() -> hec_ca

# facet the first three correspondence coordinates
hec_ca %>%
  ggbiplot() +
  facet_ord(coords = 3) +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with fixed scales and space"
  )

# facet specific correspondence coordinates by index
hec_ca %>%
  ggbiplot() +
  facet_ord(coords = c(1, 3)) +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with specific coordinates by index"
  )

# facet specific correspondence coordinates by name
hec_ca %>%
  ggbiplot() +
  facet_ord(coords = c("Can1", "Can3")) +
  geom_rows_text(aes(label = name), color = "#D55E00", size = 3) +
  geom_cols_text(aes(label = name), color = "#0072B2", size = 3) +
  ggtitle(
    "CA biplot of Snee hair & eye colors",
    "Faceted pairs with specific coordinates by name"
  )
