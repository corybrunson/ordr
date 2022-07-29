# example ordination: LRA of U.S. arrests data
arrests_lra <- ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)

# extract matrix factors
as.matrix(arrests_lra, .matrix = "rows")
as.matrix(arrests_lra, .matrix = "cols")
# special named functions
get_rows(arrests_lra)
get_cols(arrests_lra)
# get dimensions of underlying matrix factorization (not of original data)
dim(arrests_lra)

# get names of artificial / latent coordinates
get_coord(arrests_lra)
# get distribution of inertia
get_inertia(arrests_lra)
