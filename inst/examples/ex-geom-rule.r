USJudgeRatings %>%
  subset(select = -c(1, 12)) %>%
  dist(method = "maximum") %>%
  cmdscale() %>%
  as.data.frame() %>%
  setNames(c("PCo1", "PCo2")) %>%
  transform(name = rownames(USJudgeRatings)) ->
  judge_mds
USJudgeRatings %>%
  subset(select = c(CONT, RTEN)) %>%
  setNames(c("contacts", "recommendation")) ->
  judge_meta
lm(as.matrix(judge_meta) ~ as.matrix(judge_mds[, seq(2)])) %>%
  getElement("coefficients") %>%
  unname() %>% t() %>% as.data.frame() %>%
  setNames(c("Intercept", "PCo1", "PCo2")) %>%
  transform(variable = names(judge_meta)) ->
  judge_lm
ggplot(judge_mds, aes(x = PCo1, y = PCo2)) +
  coord_equal() +
  theme_void() +
  geom_text(aes(label = name), size = 3) +
  stat_rule(
    data = judge_lm, referent = judge_mds,
    aes(center = Intercept, label = variable)
  )
