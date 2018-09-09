# http://joelcadwell.blogspot.com/2015/08/matrix-factorization-comes-in-many.html

x <- scale(mtcars)
(m <- kmeans(x, 3))
plot(x, col = m$cluster)
points(m$centers, col = 1:3, pch = 8, cex = 2)

(b <- as_tbl_ord(m))
(d <- fortify(b))

# biplot with noise extending (in each artificial dimension) to the
# within-cluster (i.e. within-dimension) standard deviation
# (not great but suggestive)
coord_sdev <- augment_coord(b) %>%
  mutate(.sdev = sqrt(.withinss / .size)) %>%
  pull(.sdev)
gg <- ggbiplot(b, aes(x = 1, y = 2, color = factor(.cluster))) +
  geom_u_point(size = 3, shape = "triangle") +
  #geom_u_point(position = position_jitter(coord_sdev[1], coord_sdev[2])) +
  geom_v_vector() +
  geom_v_text_radiate(aes(label = .name))
gg
