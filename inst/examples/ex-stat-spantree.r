UScitiesD %>%
  cmdscale() %>%
  as.data.frame() %>%
  tibble::rownames_to_column(var = "city") ->
  us_mds
ggplot(us_mds, aes(-V1, -V2, label = city)) +
  stat_spantree() +
  geom_label()
