# https://stats.stackexchange.com/a/368416

## full example

plot(cmdscale(UScitiesD) %*% diag(c(-1, -1)), pch = 16L)

## prediction example

# choice of holdout city
i <- 1L
print(attr(UScitiesD, "Labels")[i])

# distance matrix of cities except holdout city
city_dist <- as.dist(as.matrix(UScitiesD)[-i, -i])

# multidimensional scaling of training cities
city_mds <- cmdscale(city_dist)

# transpose of pseudoinverse of coordinates
city_inv <- t(MASS::ginv(city_mds))

# mean squared distances from training cities
city_dsq <- apply(as.matrix(UScitiesD)[-i, -i] ^ 2, 1L, mean)

# squared distances of holdout city from other cities
i_dsq <- as.matrix(UScitiesD)[i, -i] ^ 2

# embedding coordinates of holdout city
i_pts <- -.5 * (i_dsq - city_dsq) %*% city_inv

# plot training cities
plot(city_mds, pch = 16L)
# overlay holdout city
points(i_pts, pch = 1L)
