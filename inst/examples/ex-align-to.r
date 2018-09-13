
data(country_differences)

(m <- cmdscale(country_differences, k = 2))
(b1 <- as_tbl_ord(m))
(p <- prcomp(country_differences))

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
(b2 <- as_tbl_ord(p))

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
