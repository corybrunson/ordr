# log-ratio analysis of Apollonia glass composition data
glass_apollonia <- subset(
  glass,
  Site == "Apollonia",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
glass_lra <- lra(glass_apollonia, weighted = FALSE)

# quality (cumulative proportion of inertia included)
ord_quality(glass_lra)
# adequacy (fidelity of projections to standard coordinates)
ord_adequacy(glass_lra, "rows", rank = 3)
ord_adequacy(glass_lra, "cols", rank = 3)
# adequacy (fidelity of projections to principal coordinates)
ord_predictivity(glass_lra, "dims", rank = 2)

# principal components analysis of setosa iris data
iris_pca <- princomp(iris3[, , "Setosa"])

# quality (cumulative proportion of inertia included)
ord_quality(iris_pca)
# adequacy (fidelity of projections to standard coordinates)
ord_adequacy(iris_pca, "both")
# adequacy (fidelity of projections to principal coordinates)
ord_predictivity(iris_pca, "f")
ord_predictivity(iris_pca, "g")
