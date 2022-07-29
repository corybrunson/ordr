# subset QS data to rank variables
head(qswur_usa)
qs_ranks <- subset(
  qswur_usa,
  complete.cases(qswur_usa),
  select = 8:13
)
# calculate Kendall correlation matrix
qs_cor <- cor(qs_ranks, method = "kendall")

# calculate eigendecomposition
qs_eigen <- eigen_ord(qs_cor)
# view correlations as cosines of biplot vectors
biplot(x = qs_eigen$vectors, y = qs_eigen$vectors, col = c(NA, "black"))
