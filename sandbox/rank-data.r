library(tidyverse)
library(ordr)

# -+- Put `data/raw/times.csv` in `data` folder. -+-
# https://github.com/akshi8/university_rankings

# inspect country frequencies
read_csv("data/times.csv") %>%
  pull(country) %>% table()

# read in year-rank data
read_csv("data/times.csv") %>%
  filter(country == "United States of America") %>%
  select(world_rank, university = university_name, year) %>%
  print() -> unirank

# calculate rank correlations
unirank %>%
  mutate(world_rank = as.numeric(str_extract(world_rank, "^[0-9]+"))) %>%
  spread(year, world_rank, fill = NA_real_) %>%
  drop_na() %>%
  mutate_at(vars(-1), rank, ties.method = "min") %>%
  gather("year", "rank", -1) %>%
  widyr::pairwise_cor(year, university, rank, "kendall") %>%
  spread(item2, correlation, fill = 1) %>%
  column_to_rownames("item1") %>%
  print() -> unirank_corr

# eigendecomposition of correlation matrix
unirank_corr %>%
  eigen_ord() %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> unirank_eigen
recover_conference(unirank_eigen)
# correlation biplot with unit circle
unirank_eigen %>%
  ggbiplot() +
  theme_bw() +
  geom_unit_circle(linetype = "dashed") +
  geom_u_vector() +
  geom_u_text_repel(aes(label = .name), nudge_x = .05, segment.alpha = .5) +
  scale_x_continuous(expand = c(0, .1)) +
  scale_y_continuous(expand = c(.05, .05))

# FACET EXPERIMENT

# read in year-rank data
read_csv("data/times.csv") %>%
  filter(country %in% c("United States of America", "United Kingdom")) %>%
  select(world_rank, country, university = university_name, year) %>%
  print() -> unirank

# calculate rank correlations within countries
unirank %>%
  # minimum value of any range
  mutate(world_rank = as.numeric(str_extract(world_rank, "^[0-9]+"))) %>%
  # restrict to unis with rankings in all years
  spread(year, world_rank, fill = NA_real_) %>%
  drop_na() %>%
  # within-country rankings
  group_by(country) %>%
  mutate_at(vars(-(1:2)), rank, ties.method = "min") %>%
  gather("year", "rank", -(1:2)) %>%
  # within-country rank correlations
  group_modify(~ widyr::pairwise_cor(.x, year, university, rank, "kendall")) %>%
  print() -> unirank_corr

# eigendecompositions by country, tidied and bound
# using the experimental function `dplyr::group_map()`
unirank_corr %>%
  group_map(~ spread(.x, item2, correlation, fill = 1)) %>%
  map(~ column_to_rownames(.x, "item1")) %>%
  map(eigen_ord) %>%
  map(as_tbl_ord) %>%
  map(negate_to_nonneg_orthant, .matrix = "u") %>%
  map(augment) %>%
  map(tidy) %>%
  map(mutate, year = as.integer(.name)) %>%
  map2(
    as.list(unique(unirank_corr$country)),
    function(x, y) mutate(x, country = y)
  ) %>%
  bind_rows() %>%
  print() -> unirank_ord

# faceted biplots
ggbiplot(unirank_ord) +
  theme_bw() +
  facet_wrap(~ country) +
  geom_unit_circle(linetype = "dashed") +
  geom_u_vector(aes(color = year)) +
  geom_u_text_radiate(aes(label = .name)) +
  scale_x_continuous(expand = expand_scale(mult = c(.1, .3))) +
  scale_y_continuous(expand = expand_scale(mult = c(.2, .2))) +
  ggtitle(
    "Eigenvector biplots of Kendall correlations between university rankings",
    "Within-country rank correlations based on universities ranked each year"
  )
