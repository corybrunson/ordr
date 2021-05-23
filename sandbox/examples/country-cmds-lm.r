# Multidimensional scaling of country differences and regression of attributes
# Reproduce Exhibit 4.2 in Greenacre (2010)
data(country_differences)
country_differences %>%
  cmdscale_ord(k = 2) %>%
  as_tbl_ord() %>%
  print() -> differences_cmds
differences_plot <- differences_cmds %>%
  ggbiplot(aes(x = 1, y = 2, label = .name)) +
  geom_cols_text()
differences_plot
# Reproduce Exhibit 4.5 in Greenacre (2010)
data(country_attributes)
lm(country_attributes ~ get_rows(differences_cmds)) %>%
  as_tbl_ord() %>%
  print() -> attributes_fit
differences_plot +
  geom_cols_vector(data = attributes_fit) +
  geom_cols_text_radiate(data = attributes_fit, hjust = .3)
