ggplot(USJudgeRatings, aes(x = INTG, y = PREP)) +
  geom_point() +
  stat_chull(alpha = .5)

ggplot(USJudgeRatings, aes(x = INTG, y = PREP)) +
  stat_peel(
    aes(alpha = after_stat(hull)),
    breaks = seq(.1, .9, .2), color = "black"
  )
