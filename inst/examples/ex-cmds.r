
# reproduce Exhibit 4.2 in Greenacre (2010)

# multidimensional scaling setup
data(country_differences)
(m <- cmdscale(country_differences, k = 2))
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

# reproduce Exhibit 4.5 in Greenacre (2010)

data(country_attributes)

# regress the attributes on the plotting dimensions and annotate the biplot
fit <- as_tbl_ord(lm(country_attributes ~ get_u(b)))
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
set.seed(267481)
gg +
  geom_v_vector(data = fit) +
  ggrepel::geom_text_repel(data = dat, aes(label = .name))
