# until {smoothSurv} is restored to CRAN
# remotes::install_version("smoothSurv", version = "2.5")
# remotes::install_version("icensBKL", version = "1.4")

# example biplot
# https://ibiostat.be/online-resources/icbook/supplemental

# library(icensBKL)
data("tandmob", package = "icensBKL")
# manually run biplot definition script
# https://github.com/cran/icensBKL
source("sandbox/ic-biplot/icbiplot.R")

# data management

# subset interval data to girls and three teeth
L <- subset(tandmob, fGENDER == "girl", select = c(L14, L15, L16))
R <- subset(tandmob, fGENDER == "girl", select = c(R14, R15, R16))
# inspect data
head(L)
head(R)
print(max(R, na.rm = TRUE))
# represent left-censored data by earliest possible start times
L[is.na(L)] <- 0
# represent right-censored data by a large upper bound (Cecere &al, 2013)
R[is.na(R)] <- 20

# call biplot

icb <- icbiplot(
  L, R,
  p = 2L, MaxIter = 1e4L, tol = 1e-6,
  plotit = FALSE, seed = 12345
)

# diagnostic 1

cat("Dispersion accounted for:", icb$DAF)

# diagnostic 2

orderXY <- t(apply(icb$X %*% t(icb$Y), 1, rank))
orderH <- t(apply(icb$H, 1, rank))

max(orderXY - orderH)
sum(apply(orderH, 1, function(x) any((trunc(x) - x) < 0)))

table(apply(orderXY, 1, paste, collapse = ",")) 

# regions with similar sequence

normx <- apply(icb$X, 1L, function(x) sqrt(sum(x^2)))

radians <- cbind(
  apply(orderXY, 1L, paste, collapse = ","),
  acos((icb$X / normx) %*% c(1, 0))
)
radians[, 2L] <- ifelse(
  icb$X[, 2L] < 0,
  2 * pi - as.numeric(radians[, 2L]),
  as.numeric(radians[, 2L])
)
minrad <- as.numeric(tapply(radians[, 2L], radians[, 1L], min))
maxrad <- as.numeric(tapply(radians[, 2L], radians[, 1L], max))

# plot biplot

# plot window; faint (or invisible) case markers
plot(
  icb$X[, 1L], icb$X[, 2L],
  xlim = c(-4, 1), cex = 0.5, pch = NA,
  xlab = 'Dimension 1', ylab = 'Dimension 2', col = "snow4",
  asp = 1
)

# region of common sequence
r <- 6
polygon(
  c(0, r * cos(minrad[2]), r * cos(maxrad[2])),
  c(0, r * sin(minrad[2]), r * sin(maxrad[2])), col = "gray", border = NA
)
# common sequence region boundaries
# segments(0, 0, r * cos(minrad[2]), r * sin(minrad[2]), lty = 3)
# segments(0, 0, r * cos(maxrad[2]), r * sin(maxrad[2]), lty = 3)
# box(which = "plot", lty = "solid")

# case markers
points(icb$X[, 1], icb$X[, 2], cex = 0.5, pch = 4)
# tooth markers and labels
points(icb$Y[, 1], icb$Y[, 2], cex = 1.5, pch = 20)
for (t in 1:nrow(icb$Y))
  text(icb$Y[t, 1], icb$Y[t, 2], t + 13, col = 'black', cex = 1, pos = 2)
# artificial axes
# abline(v = 0)
# abline(h = 0)

# pick one case to predict sequence for
id <- 1
# vector to case
arrows(0, 0, icb$X[id, 1], icb$X[id, 2], lwd = 2, length = 0.1, angle = 15)
# projections from teeth
for (t in 1:nrow(icb$Y)) {
  r <- (icb$Y[t, 1] * icb$X[id, 1] + icb$Y[t, 2] * icb$X[id, 2]) /
    (icb$X[id, 1]^2 + icb$X[id, 2]^2)
  segments(
    icb$Y[t, 1], icb$Y[t, 2],
    r * icb$X[id, 1], r * icb$X[id, 2], lty = 2
  )
}
