# example taken from `help(ade4::nipals)`
data(doubs, package = "ade4")
# NIPALS on sites and environmental variables
nip1 <- ade4::nipals(doubs$env)
as_tbl_ord(nip1)
# augment with row and column names
nip1 %>%
  as_tbl_ord() %>%
  augment() %>%
  print() -> nip1ord

# geographic arrangement of sites
site_plot <- cbind(site = rownames(doubs$env), doubs$xy) %>%
  tibble::as_tibble() %>%
  ggplot(aes(x = x, y = y)) +
  coord_equal() +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_path() +
  #geom_text(aes(label = site)) +
  geom_label(aes(label = ifelse(as.integer(site) %% 4L == 1L,
                                site, NA_character_)))
# biplot of sites and environmental variables
site_env_biplot <- nip1ord %>%
  ggbiplot(aes(x = 2, y = 1),
           sec.axes = "v", scale.factor = 10) +
  theme_bw() + theme(panel.grid = element_blank()) +
  geom_u_path(alpha = .6) +
  #geom_u_text(aes(label = .name), alpha = .8) +
  geom_label(aes(label = ifelse(as.integer(.name) %% 4L == 1L,
                                .name, NA_character_),
                 alpha = .6)) +
  geom_v_vector(color = "darkred") +
  geom_v_text_radiate(aes(label = .name), color = "darkred", alpha = 2/3) +
  scale_size_area(guide = "none") +
  guides(alpha = "none")
# juxtapose plots
plot(gridExtra::arrangeGrob(grobs = list(site_plot, site_env_biplot), ncol = 2))
