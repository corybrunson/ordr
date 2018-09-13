
data(finches)

finches_mat <- t(finches)

finches_lsvd <- as_tbl_ord(logisticSVD(finches_mat))
ggbiplot(finches_lsvd, aes(x = LSC1, y = LSC2)) +
  geom_u_vector(aes(x = LSC1 * .01, y = LSC2 * .01)) +
  geom_u_text(aes(x = LSC1 * .01, y = LSC2 * .01, label = .name), size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)

finches_lpca <- as_tbl_ord(logisticPCA(finches_mat))
ggbiplot(finches_lpca, aes(x = LPC1, y = LPC2)) +
  geom_u_vector(aes(x = LPC1 * .02, y = LPC2 * .02)) +
  geom_u_text_radiate(aes(x = LPC1 * .02, y = LPC2 * .02, label = .name),
                      size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)

finches_clpca <- as_tbl_ord(convexLogisticPCA(finches_mat[, -13, drop = FALSE]))
ggbiplot(finches_clpca, aes(x = LPC1, y = LPC2)) +
  geom_u_vector(aes(x = LPC1 * .02, y = LPC2 * .02)) +
  geom_u_text_radiate(aes(x = LPC1 * .02, y = LPC2 * .02, label = .name),
                      size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)
