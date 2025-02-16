ggplot(seals, aes(delta_long, delta_lat)) +
  theme_void() +
  geom_origin() +
  geom_point(alpha = .25)
