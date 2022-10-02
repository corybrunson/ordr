# savings data
class(LifeCycleSavings)
pop <- LifeCycleSavings[, 2:3]
oec <- LifeCycleSavings[, -(2:3)]
# canonical correlation analysis
savings_cca <- cancor(pop, oec)

# return the tidied canonical coefficients for the left variables
tidy(savings_cca)
# return the tidied canonical coefficients for the right variables
tidy(savings_cca, matrix = "ycoef")
# return the canonical coefficients, with summary statistics
tidy(savings_cca, matrix = "cor")

# scree plot
ggplot(tidy(savings_cca, matrix = "cor"), aes(x = CC, y = percent)) +
  theme_bw() +
  geom_col() +
  labs(x = "Canonical dimension", y = "Percent of variance")
