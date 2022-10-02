# LRA of arrest data
ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)

# CMDS of inter-city distance data
ordinate(UScitiesD, cmdscale_ord, k = 3L)

# PCA of iris data
ordinate(iris, princomp, cols = -Species, augment = c(Sepal.Width, Species))
ordinate(iris, cols = 1:4, ~ prcomp(., center = TRUE, scale. = TRUE))

# CA of hair & eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
ordinate(haireye, MASS::corresp, cols = everything())

# FA of Swiss social data
ordinate(swiss, model = factanal, factors = 2L, scores = "Bartlett")

# LDA of iris data
ordinate(iris, ~ lda_ord(.[, 1:4], .[, 5], ret.x = TRUE))

# CCA of savings data
ordinate(
  LifeCycleSavings[, c("pop15", "pop75")],
  # second data set must be handled as an additional parameter to `model`
  y = LifeCycleSavings[, c("sr", "dpi", "ddpi")],
  model = cancor_ord, scores = TRUE
)
