# table of hair and eye color data collapsed by sex
class(HairEyeColor)
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
print(haireye)
# use correspondence analysis to construct row and column profiles
haireye %>%
  ca::ca() %>%
  as_tbl_ord() %>%
  print() -> haireye_ca
# recover row and column profiles
get_rows(haireye_ca)
get_cols(haireye_ca)
# augment profiles with names, masses, distances, and inertias
(haireye_ca <- augment_ord(haireye_ca))
