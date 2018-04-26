devtools::load_all()

data(bioenv)
bioenv <- bioenv %>%
  mutate(
    x = (Depth - mean(Depth)) / sd(Depth),
    y = (Pollution - mean(Pollution)) / sd(Pollution)
  ) %>%
  mutate_at(vars(a:e), funs(p = ifelse(. > 0, 1L, 0L)))

m <- glm(
  formula = d ~ x + y, family = gaussian(power(.25)), data = bioenv,
  start = c(0, 0, 0)
)
# DEBUG
m <- glm(
  formula = d ~ x + y, family = poisson(log), data = bioenv,
  start = c(0, 0, 0)
)
# ref. Greenacre, Computational Appendix

m <- glm(formula = d_p ~ x + y, family = binomial(logit), data = bioenv)
a <- as_bibble(m)
b <- bibble(m)
#b <- bind_u(bibble(m), select(bioenv, site))
# then use aes(label = site)
# implement transformations in 'geom_*_isolines()'
ggbiplot(b, aes(x = x, y = y)) +
  geom_u_point() +
  geom_v_vector() +
  geom_v_isolines(ids = 1, by = .5)
