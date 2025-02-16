# petroleum rock base plot
p <- ggplot(rock, aes(area, shape, size = peri)) + theme_bw()
# scatterplot
p + geom_point()
# NB: Non-standard aesthetics are handled as in version > 3.5.1; see:
# https://github.com/tidyverse/ggplot2/issues/6191
# custom bag fraction, coefficient, and aesthetics
p + stat_bagplot(fraction = .4, coef = 1.5,
                 outlier_gp = list(shape = "asterisk"))
# invisible fence
p + stat_bagplot(fence = FALSE)
