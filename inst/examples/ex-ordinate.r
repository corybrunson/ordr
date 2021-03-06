# PCA of iris data
ordinate(iris, -Species, princomp, augment = c(Sepal.Width, Species))
ordinate(iris, 1:4, ~ prcomp(., center = TRUE, scale. = TRUE))
# LRA of arrest data
ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)
# CA of hair & eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
ordinate(haireye, cols = everything(), model = ca::ca, augment = everything())
# lSVD of finches data
data(finches, package = "cooccur")
ordinate(finches, everything(), logisticSVD_ord)
