
data(finches)

finches_lsvd <- finches %>%
  select(-Island) %>% as.matrix() %>%
  logisticPCA::logisticSVD()
# SUBMIT ISSUE/PR TO *logisticPCA*
finches_bibble <- finches_lsvd %>%
  as_bibble() %>%
  bind_cols_u(island = finches$Island) %>%
  bind_cols_v(species = names(select(finches, -Island)))

# ILLUSTRATE ROLE-REVERSAL BETWEEN U AND V
ggbiplot(finches_bibble, aes(x = SC1, y = SC2)) +
  geom_u_vector(aes(x = SC1 * .01, y = SC2 * .01)) +
  geom_u_text(aes(x = SC1 * .01, y = SC2 * .01, label = island), size = 3) +
  geom_v_label(aes(label = species), size = 3, alpha = .5)
