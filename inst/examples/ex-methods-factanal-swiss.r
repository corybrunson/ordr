# data frame of Swiss fertility and socioeconomic indicators
class(swiss)
head(swiss)
# perform factor analysis
swiss_fa <- factanal(~ ., factors = 2L, data = swiss)
