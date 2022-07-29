# table of hair and eye color data collapsed by sex
data(quine, package = "MASS")
class(quine)
head(quine)

# use correspondence analysis to construct row and column profiles
(quine_ca <- MASS::corresp(~ Age + Eth, data = quine))
(quine_ca <- as_tbl_ord(quine_ca))

# recover row and column profiles
get_rows(quine_ca)
get_cols(quine_ca)

# augment profiles with names, masses, distances, and inertias
(quine_ca <- augment_ord(quine_ca))
