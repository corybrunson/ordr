
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
gg +
  geom_v_vector(data = fit) +
  ggrepel::geom_text_repel(data = dat, aes(label = .name))

# reproduce Exhibit 4.6 in Greenacre (2010)

data(bioenv)

# chi-squared distances
chidist <- function(mat, rowcol = 1) {
  if (rowcol == 1) {
    prof <- mat / apply(mat, 1, sum)
    rootaveprof <- sqrt(apply(mat, 2, sum) / sum(mat))
  }
  if (rowcol == 2) {
    prof <- t(mat) / apply(mat, 2, sum)
    rootaveprof <- sqrt(apply(mat, 1, sum) / sum(mat))
  }
  dist(scale(prof, FALSE, rootaveprof))
}

# `tbl_ord` MDS object
bioenv_chidist <- chidist(bioenv[, 2:6])
bioenv_cmds <- as_tbl_ord(cmdscale(bioenv_chidist))
bioenv_cmds <- mutate_u(bioenv_cmds, .name = bioenv$site)

# sites plot
ggbiplot(bioenv_cmds, aes(label = .name)) +
  geom_u_text()

# sites and species biplot
bioenv_rf <- diag(1 / rowSums(bioenv[, 2:6])) %*%
  as.matrix(bioenv[, 2:6]) %*%
  diag(sqrt(sum(bioenv[, 2:6]) / colSums(bioenv[, 2:6])))
colnames(bioenv_rf) <- names(bioenv)[2:6]
bioenv_lm <- as_tbl_ord(lm(bioenv_rf ~ get_u(bioenv_cmds)))
ggbiplot(bioenv_cmds, aes(label = .name)) +
  geom_u_text() +
  geom_v_vector(data = bioenv_lm) +
  geom_v_text_radiate(data = bioenv_lm)
