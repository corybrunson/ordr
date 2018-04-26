devtools::load_all()

data(country_differences, country_attributes)
m1 <- country_differences %>%
  select(-Countries) %>%
  as.matrix()
m2 <- country_attributes %>%
  select(-Countries) %>%
  as.matrix()

# small version
mm <- m1[1:5, 1:5]

x <- cmdscale(mm, k = 3)
# principal coordinates
x$points
-2 * x$points %*% t(x$points)
# doubly centered symmetric distance matrix
x$x

# reproduce text
# squared proximity matrix
D <- mm^2
# centering matrix
J <- diag(nrow(mm)) - 1/nrow(mm)
# doubly-centered symmetric distance matrix
B <- J %*% D %*% J
eig <- eigen(-.5 * B)
# matrix of eigenvectors
E <- eig$vectors
# matrix of principal coordinates
X <- E %*% diag(sqrt(eig$values))
# recover distance matrix
-2 * X[, 1:3] %*% t(X[, 1:3])

# full version
x <- cmdscale(m1, k = 2)
# principal coordinates
x$points
-2 * x$points %*% t(x$points)
# doubly centered symmetric distance matrix
x$x
# residuals
hist(-2 * x$points %*% t(x$points) - x$x)
# MDS plot
plot(x = x$points, pch = NA)
text(x = x$points, label = colnames(x$x))
# regress attributes on principal axes
# function 'regress_u()'?
fit <- lm(m2 ~ x$points)
arrows(
  0, 0, fit$coefficients[2, ], fit$coefficients[3, ],
  col = "darkred", lwd = 2, length = .1, angle = 15
)
text(
  t(fit$coefficients[2:3, ]),
  col = "darkred", cex = .75, labels = colnames(fit$coefficients)
)

# chi-square distance biplot
data(bioenv)
# use chi-squared method
mds_fit <- bioenv %>% select(a:e) %>% dist() %>% cmdscale(k = 2)
plot(x = mds_fit$points, pch = NA)
text(x = mds_fit$points, label = bioenv$site)
species_data <- bioenv %>% select(a:e) %>% as.matrix()
mlm_fit <- lm(species_data ~ mds_fit$points)
arrows(
  0, 0, mlm_fit$coefficients[2, ], mlm_fit$coefficients[3, ],
  col = "darkred", lwd = 2, length = .1, angle = 15
)
text(
  t(mlm_fit$coefficients[2:3, ]),
  col = "darkred", cex = .75, labels = colnames(mlm_fit$coefficients)
)
