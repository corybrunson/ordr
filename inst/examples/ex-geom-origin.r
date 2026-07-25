ggplot() +
  theme_void() +
  geom_origin() +
  geom_point(data = seals, aes(delta_long, delta_lat), alpha = .25)
