
# http://factominer.free.fr/course/missing.html
data(orange, package = "missMDA")
nb <- missMDA::estim_ncpPCA(orange, ncp.max = 5)

res.em <- missMDA::imputePCA(orange, ncp = nb$ncp, method = "EM")
res.reg <- missMDA::imputePCA(orange, ncp = nb$ncp, method = "Reg")

hist((res.reg$completeObs - res.em$completeObs)[which(is.na(orange))])
hist((res.reg$fittedX - res.em$fittedX)[which(is.na(orange))])

res.em.pca <- prcomp(res.em$completeObs, scale. = TRUE)
res.reg.pca <- prcomp(res.reg$completeObs, scale. = TRUE)

res.em.pca %>%
  as_tbl_ord() %>%
  augment() %>%
  confer_inertia(.5) %>%
  ggbiplot(aes(label = .name)) +
  theme_bw() +
  geom_v_vector(color = "darkgrey") +
  geom_u_point() +
  geom_u_text_repel()
