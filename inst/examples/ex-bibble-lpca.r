
data(finches)

finches_mat <- finches %>%
  select(-Island) %>% as.matrix()
rownames(finches_mat) <- finches$Island

finches_lsvd <- finches_mat %>%
  logisticSVD() %>%
  as_bibble()
# ILLUSTRATE ROLE-REVERSAL BETWEEN U AND V
ggbiplot(finches_lsvd, aes(x = LSC1, y = LSC2)) +
  geom_u_vector(aes(x = LSC1 * .01, y = LSC2 * .01)) +
  geom_u_text(aes(x = LSC1 * .01, y = LSC2 * .01, label = .name), size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)

finches_lpca <- finches_mat %>%
  logisticPCA() %>%
  as_bibble()
ggbiplot(finches_lpca, aes(x = LPC1, y = LPC2)) +
  geom_u_vector(aes(x = LPC1 * .01, y = LPC2 * .01)) +
  geom_u_text(aes(x = LPC1 * .01, y = LPC2 * .01, label = .name), size = 3) +
  geom_v_label(aes(label = .name), size = 3, alpha = .5)
