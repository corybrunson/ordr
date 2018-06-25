# reproduce Exhibit 4.5 in Greenacre (2010)

data(country_differences, country_attributes)
# format both tibbles as matrices
x1 <- as.matrix(dplyr::select(country_differences, -Countries))
rownames(x1) <- dplyr::pull(country_differences, Countries)
x2 <- as.matrix(dplyr::select(country_attributes, -Countries))
rownames(x2) <- dplyr::pull(country_differences, Countries)

# multidimensional scaling setup
(m <- cmdscale(x1, k = 2))
(b <- as_tbl_ord(m))
(d <- fortify(b))

# basic multidimensional scaling biplot
gg <- ggbiplot(b, aes(x = PCo1, y = PCo2)) +
  geom_v_text(aes(label = .name))
gg
# default aesthetic assignments
ggbiplot(b) +
  geom_v_text(aes(label = .name))
# numeric aesthetic assignments
ggbiplot(b, aes(x = 2, y = 1)) +
  geom_v_text(aes(label = .name))
# regress the attributes on the plotting dimensions and annotate the biplot
fit <- as_tbl_ord(lm(x2 ~ get_u(b)))
gg +
  geom_v_vector(data = fit) +
  geom_v_text_radiate(data = fit, hjust = .3, aes(label = .name))
gg +
  geom_v_vector(data = fit) +
  geom_v_text_radiate(data = fortify(fit, include = "all"), hjust = .3,
                   aes(label = .name))
gg +
  geom_v_vector(data = fit) +
  geom_v_text_radiate(data = fortify(fit, "v"), hjust = .3, aes(label = .name))

dat <- fortify_v(fit)
gg +
  geom_v_vector(data = fit) +
  geom_text(data = dat, aes(label = .name))