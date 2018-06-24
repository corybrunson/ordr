
data(finches)

finches_mat <- as.matrix(dplyr::select(finches, -Island))
rownames(finches_mat) <- finches$Island

finches_lsvd <- as_tbl_ord(logisticSVD(finches_mat))
# ILLUSTRATE ROLE-REVERSAL BETWEEN U AND V
ggbiplot(finches_lsvd, aes(x = LSC1, y = LSC2)) +
  geom_u_vector(aes(x = LSC1 * .01, y = LSC2 * .01)) +
  geom_u_text(aes(x = LSC1 * .01, y = LSC2 * .01, label = .name), size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)

finches_lpca <- as_tbl_ord(logisticPCA(finches_mat))
# DISTINGUISH SEGMENT SIZE FROM TEXT SIZE (EXTRA `cex` PARAMETER?)
ggbiplot(finches_lpca, aes(x = LPC1, y = LPC2)) +
  geom_u_axis(aes(x = LPC1 * .02, y = LPC2 * .02, label = .name), size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)
