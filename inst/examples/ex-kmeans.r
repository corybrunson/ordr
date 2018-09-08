# http://joelcadwell.blogspot.com/2015/08/matrix-factorization-comes-in-many.html

x <- rbind(matrix(rnorm(100, sd = 0.3), ncol = 2),
           matrix(rnorm(100, mean = 1, sd = 0.3), ncol = 2))
colnames(x) <- c("x", "y")
(m <- kmeans(x, 2))
plot(x, col = m$cluster)
points(m$centers, col = 1:2, pch = 8, cex = 2)

(b <- as_tbl_ord(m))
(d <- fortify(b))

# biplot with noise extending (in each artificial dimension) to the
# within-cluster (i.e. within-dimension) standard deviation
# (not great but suggestive)
coord_sdev <- augment_coord(b) %>%
  mutate(.sdev = sqrt(.withinss / .size)) %>%
  pull(.sdev)
gg <- ggbiplot(b, aes(x = 1, y = 2)) +
  geom_u_point(position = position_jitter(coord_sdev[1], coord_sdev[2])) +
  geom_v_vector() +
  geom_v_text_radiate(aes(label = .name))
gg

# same procedure, but centering the data before clustering
xx <- scale(x, center = TRUE, scale = FALSE)
(mm <- kmeans(xx, 2))
(bb <- as_tbl_ord(mm))
(dd <- fortify(bb))
plot(xx, col = mm$cluster)
points(mm$centers, col = 1:2, pch = 8, cex = 2)
coord_sdev <- augment_coord(bb) %>%
  mutate(.sdev = sqrt(.withinss / .size)) %>%
  pull(.sdev)
gg <- ggbiplot(bb, aes(x = 1, y = 2)) +
  geom_u_point(position = position_jitter(coord_sdev[1], coord_sdev[2])) +
  geom_v_vector() +
  geom_v_text_radiate(aes(label = .name))
gg
