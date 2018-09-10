
data(bioenv)

# reproduce Exhibit 2.5 in Greenacre (2010)

# standardize all (numeric) variables
bioenv_std <- dplyr::mutate_if(bioenv, is.numeric, scale)

# multiple linear regression setup
resp <- as.matrix(dplyr::select(bioenv_std, a:e))
pred <- as.matrix(dplyr::select(bioenv_std, x = Depth, y = Pollution))
(m <- lm(resp ~ pred + 0))
(b <- as_tbl_ord(m))

# multiple linear regression biplot
ggbiplot(b, aes(label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4")

# reproduce Exhibit 3.2 in Greenacre (2010)

# standardize the predictors and fourth-root transform the response variables
bioenv_4rt <- dplyr::mutate_at(bioenv, dplyr::vars(a:e), dplyr::funs(. ^ (1/4)))
bioenv_4rt <- dplyr::mutate_at(bioenv_4rt, dplyr::vars(Pollution:Depth), scale)

# multiple linear regression setup
resp <- as.matrix(dplyr::select(bioenv_4rt, a:e))
pred <- as.matrix(dplyr::select(bioenv_4rt, x = Depth, y = Pollution))
(m <- lm(resp ~ pred))
(b <- as_tbl_ord(m))

# multiple linear regression biplot
ggbiplot(b, aes(x = x, y = y, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4")

# multiple linear regression biplot
ggbiplot(b, aes(x = x, y = y, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4")

# reproduce (3.2) in Greenacre (2010)

# standardize the predictors
bioenv_pois <- dplyr::mutate(bioenv, x = scale(Depth), y = scale(Pollution))

# single generalized linear regression setup
(m <- glm(d ~ x + y, "poisson", bioenv_pois))
(b <- as_tbl_ord(m))

# single generalized linear regression biplot
ggbiplot(b, aes(x = x, y = y, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4")

# reproduce (3.4) and Exhibit 3.6 in Greenacre (2010)

# standardize the predictors and binarize the count responses
bioenv_bin <- dplyr::mutate_at(bioenv, dplyr::vars(a:e), as.logical)
bioenv_bin <- dplyr::mutate(bioenv_bin, x = scale(Depth), y = scale(Pollution))

# multiple generalized linear regression setup
#resp <- as.matrix(dplyr::select(bioenv_std, a:e))
#pred <- as.matrix(dplyr::select(bioenv_std, x = Depth, y = Pollution))
#(m <- glm(resp ~ pred, "binomial"))

# single generalized linear regression setup
(m <- glm(d ~ x + y, "binomial", bioenv_bin))
(b <- as_tbl_ord(m))

# check link function
family(b)$linkfun
family(b)$linkinv
# can/should `family()` be passed through `ggplot()` and on to `geom_*()`?

# single generalized linear regression biplot
# ('intercept' aesthetic is vital for linking GLM parameter to coordinates)
ggbiplot(b, aes(x = x, y = y, intercept = `(Intercept)`, label = .name)) +
  theme_bw() +
  geom_u_text(color = "darkgreen") +
  geom_v_vector(color = "brown4") +
  geom_v_text_radiate(color = "brown4") +
  geom_v_isolines(family = binomial(), by = .05) +
  ylim(c(-2.7, 2.7))
