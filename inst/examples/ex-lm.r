
# standardize two predictors
bioenv <- dplyr::mutate(
  bioenv,
  x = (Depth - mean(Depth)) / sd(Depth),
  y = (Pollution - mean(Pollution)) / sd(Pollution)
)

# linear regression setup
(m <- lm(data = bioenv, formula = d ~ x + y))
(b <- as_tbl_ord(m))
(d <- tidy(b))
