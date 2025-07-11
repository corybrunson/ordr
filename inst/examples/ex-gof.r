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
ord_adequacy(glass_lra, "rows")
ord_adequacy(glass_lra, "cols")
# adequacy (fidelity of projections to principal coordinates)
ord_predictivity(glass_lra, "rows")
ord_predictivity(glass_lra, "cols")

# FIXME: Predictability throws error on empty elements.

# principal components analysis of setosa iris data
iris_pca <- princomp(iris3[, , "Setosa"])

# quality (cumulative proportion of inertia included)
ord_quality(iris_pca)
# adequacy (fidelity of projections to standard coordinates)
ord_adequacy(iris_pca, "rows")
ord_adequacy(iris_pca, "cols")
# adequacy (fidelity of projections to principal coordinates)
ord_predictivity(iris_pca, "rows")
ord_predictivity(iris_pca, "cols")
