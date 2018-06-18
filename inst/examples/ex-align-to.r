
data(country_differences, country_attributes)

x1 <- as.matrix(dplyr::select(country_differences, -Countries))
rownames(x1) <- dplyr::pull(country_differences, Countries)

(m <- cmdscale(x1, k = 2))
(b1 <- as_bibble(m))
(p <- prcomp(x1))

\dontrun{
(b1_p <- align_to(b1, p$x, "u"))
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(b1) + geom_u_point(),
  ggbiplot(b1_p) + geom_u_point()
), ncol = 2))

(b1_p <- align_to(b1, p$rotation, "v"))
plot(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(b1) + geom_u_point(),
  ggbiplot(b1_p) + geom_u_point()
), ncol = 2))

# Run once methods are implemented for 'prcomp' objects
(b2 <- as_bibble(p))

print(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(b1) + geom_u_point(),
  ggbiplot(b2) + geom_u_point()
), ncol = 2))

(b1_p <- align_to(b1, p$rotation, "v"))

print(gridExtra::arrangeGrob(grobs = list(
  ggbiplot(b1_p) + geom_u_point(),
  ggbiplot(b2) + geom_u_point()
), ncol = 2))
}
