# reproduce Exhibit 7.1 in Greenacre (2010)

# manually construct singular value decomposition
x <- as.matrix(USArrests[, -3])
r <- (1 / sum(x)) * x %*% matrix(1, ncol(x))
c <- (1 / sum(x)) * t(x) %*% matrix(1, nrow(x))
l <- log(x)
y <- (diag(nrow(x)) - matrix(1, nrow(x)) %*% t(r)) %*%
  l %*%
  t(diag(ncol(x)) - matrix(1, ncol(x)) %*% t(c))
d_r <- diag(1 / nrow(x), nrow(x))
d_c <- diag(1 / ncol(x), ncol(x))
s <- sqrt(d_r) %*% y %*% sqrt(d_c)
dimnames(s) <- dimnames(x)
d <- svd(s)

# manually compute principal and standard coordinates
f <- diag(1 / sqrt(diag(d_r))) %*% d$u %*% diag(d$d)
g <- diag(1 / sqrt(diag(d_c))) %*% d$v %*% diag(d$d)
phi <- diag(1 / sqrt(diag(d_r))) %*% d$u
gamma <- diag(1 / sqrt(diag(d_c))) %*% d$v
sc <- 5
plot(rbind(f, gamma / sc), pch = NA)
points(f, pch = 16, col = "darkgreen")
points(gamma / sc, pch = 17, col = "brown4")
lines(rbind(gamma, gamma[1, ]) / sc, lty = 3, lwd = 2, col = "brown4")

# weighted SVD as 'tbl_ord'
(b <- augment(as_tbl_ord(d)))
get_conference(b)

b <- confer_inertia(b, 0)
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen", size = 3) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(color = "brown4", fontface = "bold")

# using a secondary axis
b <- confer_inertia(b, 1)
ggbiplot(b, aes(label = .name)) +
  geom_u_text(color = "darkgreen", size = 3) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(color = "brown4", fontface = "bold")
ggbiplot(b, aes(label = .name), sec.axes = "v") +
  geom_u_text(color = "darkgreen", size = 3) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(color = "brown4", fontface = "bold")
ggbiplot(b, aes(label = .name), sec.axes = "u") +
  geom_u_text(color = "darkgreen", size = 3) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(color = "brown4", fontface = "bold")
ggbiplot(b, aes(label = .name), sec.axes = "u", scale.factor = 10) +
  geom_u_text(color = "darkgreen", size = 3) +
  geom_v_polygon(fill = NA, linetype = "dashed", color = "brown4") +
  geom_v_text(color = "brown4", fontface = "bold")
# NOTE: SECONDARY AXES ARE ADDED INTERNALLY USING `scale_*_continuous()`;
# THEY WILL BE REPLACED BY DOWNSTREAM `x` AND `y` SCALES.
