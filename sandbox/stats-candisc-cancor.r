cca1 <- cancor_ord(
  LifeCycleSavings[, c("pop15", "pop75")],
  LifeCycleSavings[, c("sr", "dpi", "ddpi")]
)
cca2 <- candisc::cancor(
  LifeCycleSavings[, c("pop15", "pop75")],
  LifeCycleSavings[, c("sr", "dpi", "ddpi")]
)
cca2$coef$X / cca1$xcoef
cca2$coef$Y / cca1$ycoef[, 1:2]
# scale factor = 7

glass_banias <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("SiO2", "Na2O", "CaO", "Al2O3", "MgO", "K2O")
)
glass_banias_minor <- subset(
  glass,
  Context == "L.15;B.166",
  select = c("TiO2", "FeO", "MnO", "P2O5", "Cl", "SO3")
)
nc <- 2L
cca1 <- cancor_ord(glass_banias[, seq(nc)], glass_banias_minor[, seq(nc)])
cca2 <- candisc::cancor(glass_banias[, seq(nc)], glass_banias_minor[, seq(nc)])
cca2$coef$X / cca1$xcoef
cca2$coef$Y / cca1$ycoef
# scale factor = 2.236068

# where is the scale multiple coming from?
# why isn't it roughly constant when `nc = 5`?
