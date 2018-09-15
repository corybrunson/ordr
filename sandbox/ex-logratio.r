# reproduce Exhibit 7.1 in Greenacre (2010)

x <- as.matrix(USArrests[, -3])
r <- (1 / sum(x)) * x %*% matrix(1, ncol(x))
c <- (1 / sum(x)) * t(x) %*% matrix(1, nrow(x))
l <- log(x)
y <- (diag(nrow(x)) - matrix(1, nrow(x)) %*% t(r)) %*%
  l %*%
  t(diag(ncol(x)) - matrix(1, ncol(x)) %*% t(c))
s <- diag(sqrt(1 / nrow(x)), nrow(x)) %*% y %*% diag(sqrt(1 / ncol(x)), ncol(x))
dimnames(s) <- dimnames(x)
d <- svd(s)
b <- augment(as_tbl_ord(d))

# make this a secondary axis example

b <- confer_inertia(b, 0)
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_text(color = "brown4", fontface = "bold")

b <- confer_inertia(b, 1)
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_text(aes(x = SV1 / 20, y = SV2 / 20), color = "brown4", fontface = "bold")
