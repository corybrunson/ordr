data(finches)
# calculate pairwise and full-partial correlation-distance matrices
pw_cor <- psych::tetrachoric(finches)$rho
pt_cor <- psych::partial.r(pw_cor)
# perform multidimensional scaling on both and ensure alignment
pw_bbl <- pw_cor %>% {sqrt(2 * (1 - .))} %>%
  cmdscale(k = 2) %>% as_bibble()
pt_bbl <- pt_cor %>% {sqrt(2 * (1 - .))} %>%
  cmdscale(k = 2) %>% as_bibble()

biplots <- lapply(list(pw_bbl, pt_bbl), function(b) {
  ggbiplot(b, aes(x = PCo1, y = PCo2, label = abbreviate(name))) +
    theme_bw() +
    geom_u_vector(color = "darkgrey") +
    geom_u_text_repel()
})
plot(gridExtra::arrangeGrob(grobs = biplots, nrow = 1, respect = FALSE))

pt_bbl_aligned <- pt_bbl %>% align_with(pw_bbl, matrix = "u")
biplots[[2]] <- pt_bbl_aligned %>%
  ggbiplot(aes(x = PCo1, y = PCo2, label = abbreviate(name))) +
  theme_bw() +
  geom_u_vector(color = "darkgrey") +
  geom_u_text_repel()
plot(gridExtra::arrangeGrob(grobs = biplots, nrow = 1, respect = FALSE))
