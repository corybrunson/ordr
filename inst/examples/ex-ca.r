
data(smoke, package = "ca")

(m <- ca::ca(smoke))
(b <- as_tbl_ord(m))
(d <- fortify(b))

plot(m, map = "rowprincipal", col = c("green", "brown"))

# confer singular values to reproduce the row-principal biplot
b <- confer_inertia(b, "rowprincipal")
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_point(color = "green") +
  geom_u_text_repel(color = "blue") +
  geom_v_point(color = "brown", shape = 17) +
  geom_v_text_repel(color = "brown")
