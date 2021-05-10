# PCA of iris data
ordinate(iris, 1:4, prcomp)
ordinate(iris, -Species, princomp, augment = c(Sepal.Width, Species))
# LRA of arrest data
ordinate(USArrests, cols = c(Murder, Rape, Assault), lra)
# CA of hair & eye color data
haireye <- as.data.frame(rowSums(HairEyeColor, dims = 2L))
ordinate(haireye, cols = everything(), model = ca::ca, augment = everything())
