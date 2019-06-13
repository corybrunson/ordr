# Adapt Exhibit 4.6 in Greenacre (2010)
ycols <- 2:6
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
# multidimensional scaling of species counts by chi-squared distances
bioenv[, ycols] %>%
  chidist() %>%
  cmdscale() %>%
  as_tbl_ord() %>%
  mutate_u(.name = bioenv$site) %>%
  print() -> bioenv_cmds
# regress species relative frequencies on principal coordinates
bioenv[, ycols] %>%
  sweep(1, 1 / rowSums(bioenv[, ycols]), "*") %>%
  sweep(2, sqrt(sum(bioenv[, ycols]) / colSums(bioenv[, ycols])), "*") %>%
  as.matrix() -> bioenv_relfreq
lm(bioenv_relfreq ~ get_u(bioenv_cmds)) %>%
  as_tbl_ord() %>%
  print() -> bioenv_lm
# biplot of species with regression vectors onto principal coordinates
ggbiplot(bioenv_cmds, aes(label = .name)) +
  geom_u_text(color = "seagreen") +
  geom_v_vector(data = bioenv_lm, color = "darkred") +
  geom_v_text_repel(data = bioenv_lm)
