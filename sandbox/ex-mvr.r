data(gasoline, package = "pls")

(m <- pls::plsr(octane ~ NIR, ncomp = 10, data = gasoline))
summary(m)
plot(m, ncomp = 2)

(b <- as_tbl_ord(m))

(b <- designate(b, pred))
ggbiplot(b, aes(x = 1, y = 2)) +
  geom_u_point() +
  geom_v_vector()

(b <- designate(b, "resp"))
ggbiplot(b, aes(x = 1, y = 2)) +
  geom_u_point() +
  geom_v_vector()
# want to be able to decide which coordinates to render layers for
