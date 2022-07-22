factanal_fit_mle_visible <- function (
    cmat, factors, start = NULL, lower = 0.005, control = NULL, 
    ...
) {
  FAout <- function(Psi, S, q) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1L:q, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
    # intuitively, return the square root of the correlation matrix
    # res * t(res) = S
    diag(sqrt(Psi)) %*% load
  }
  FAfn <- function(Psi, S, q) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
    e <- E$values[-(1L:q)]
    e <- sum(log(e) - e) - q + nrow(S)
    -e
  }
  FAgr <- function(Psi, S, q) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1L:q, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), q)
    load <- diag(sqrt(Psi)) %*% load
    g <- load %*% t(load) + diag(Psi) - S
    diag(g)/Psi^2
  }
  p <- ncol(cmat)
  if (is.null(start)) 
    start <- (1 - 0.5 * factors/p)/diag(solve(cmat))
  res <- optim(
    start, FAfn, FAgr, method = "L-BFGS-B", lower = lower, upper = 1,
    control = c(
      list(fnscale = 1, parscale = rep(0.01, length(start))),
      control
    ),
    q = factors, S = cmat
  )
  Lambda <- FAout(res$par, cmat, factors)
  dimnames(Lambda) <- list(dimnames(cmat)[[1L]], paste0("Factor", 1L:factors))
  p <- ncol(cmat)
  dof <- 0.5 * ((p - factors)^2 - p - factors)
  un <- setNames(res$par, colnames(cmat))
  class(Lambda) <- "loadings"
  ans <- list(
    converged = res$convergence == 0, loadings = Lambda, 
    uniquenesses = un, correlation = cmat,
    criteria = c(objective = res$value, counts = res$counts),
    factors = factors, dof = dof,
    method = "mle"
  )
  class(ans) <- "factanal"
  ans
}

cmat <- structure(c(
  1, 0.353079183619975, -0.645882706457287, -0.663788857035069, 
  0.463684700651794, 0.416556031434983, 0.353079183619975, 1, -0.686542208617136, 
  -0.63952251894832, 0.40109505304874, -0.0608586084165816, -0.645882706457287, 
  -0.686542208617136, 1, 0.698415296288483, -0.572741806064167, 
  -0.114021596482034, -0.663788857035069, -0.63952251894832, 0.698415296288483, 
  1, -0.153858917090915, -0.0993218458213457, 0.463684700651794, 
  0.40109505304874, -0.572741806064167, -0.153858917090915, 1, 
  0.175495907596623, 0.416556031434983, -0.0608586084165816, -0.114021596482034, 
  -0.0993218458213457, 0.175495907596623, 1), .Dim = c(6L, 6L), .Dimnames = list(
    c(
      "Fertility", "Agriculture", "Examination", "Education", 
      "Catholic", "Infant.Mortality"),
    c("Fertility", "Agriculture", "Examination", "Education",
      "Catholic", "Infant.Mortality")
  ))
start <- c(Fertility = 0.244387498672728, Agriculture = 0.318306172779521, 
           Examination = 0.221160398000011, Education = 0.193462271025516, 
           Catholic = 0.354736362106193, Infant.Mortality = 0.630071494294487
)
nfit <- factanal_fit_mle_visible(
  cmat = cmat, factors = 2L, start = start, lower = 0.005, control = NULL
)

factanal_visible <- function (
    x, factors, data = NULL, covmat = NULL, n.obs = NA, 
    subset, na.action, start = NULL,
    scores = c("none", "regression", "Bartlett"), rotation = "varimax",
    control = NULL, ...
) {
  sortLoadings <- function(Lambda) {
    cn <- colnames(Lambda)
    Phi <- attr(Lambda, "covariance")
    ssq <- apply(Lambda, 2L, function(x) -sum(x^2))
    Lambda <- Lambda[, order(ssq), drop = FALSE]
    colnames(Lambda) <- cn
    neg <- colSums(Lambda) < 0
    Lambda[, neg] <- -Lambda[, neg]
    if (!is.null(Phi)) {
      unit <- ifelse(neg, -1, 1)
      attr(Lambda, "covariance") <- unit %*% Phi[order(ssq), 
                                                 order(ssq)] %*% unit
    }
    Lambda
  }
  cl <- match.call()
  na.act <- NULL
  if (is.list(covmat)) {
    if (anyNA(match(c("cov", "n.obs"), names(covmat)))) 
      stop("'covmat' is not a valid covariance list")
    cv <- covmat$cov
    n.obs <- covmat$n.obs
    have.x <- FALSE
  } else if (is.matrix(covmat)) {
    cv <- covmat
    have.x <- FALSE
  } else if (is.null(covmat)) {
    if (missing(x)) 
      stop("neither 'x' nor 'covmat' supplied")
    have.x <- TRUE
    if (inherits(x, "formula")) {
      mt <- terms(x, data = data)
      if (attr(mt, "response") > 0) stop("response not allowed in formula")
      attr(mt, "intercept") <- 0
      mf <- match.call(expand.dots = FALSE)
      names(mf)[names(mf) == "x"] <- "formula"
      mf$factors <- mf$covmat <- mf$scores <- mf$start <- mf$rotation <- mf$control <- mf$... <- NULL
      mf[[1L]] <- quote(stats::model.frame)
      mf <- eval.parent(mf)
      na.act <- attr(mf, "na.action")
      if (stats:::.check_vars_numeric(mf)) 
        stop("factor analysis applies only to numerical variables")
      z <- model.matrix(mt, mf)
    } else {
      z <- as.matrix(x)
      if (!is.numeric(z)) 
        stop("factor analysis applies only to numerical variables")
      if (!missing(subset)) 
        z <- z[subset, , drop = FALSE]
    }
    covmat <- cov.wt(z)
    cv <- covmat$cov
    n.obs <- covmat$n.obs
  } else stop("'covmat' is of unknown type")
  scores <- match.arg(scores)
  if (scores != "none" && !have.x) 
    stop("requested scores without an 'x' matrix")
  p <- ncol(cv)
  if (p < 3) 
    stop("factor analysis requires at least three variables")
  dof <- 0.5 * ((p - factors)^2 - p - factors)
  if (dof < 0) 
    stop(sprintf(ngettext(factors, "%d factor is too many for %d variables", 
                          "%d factors are too many for %d variables"),
                 factors, p), domain = NA)
  sds <- sqrt(diag(cv))
  cv <- cv/(sds %o% sds)
  cn <- list(nstart = 1, trace = FALSE, lower = 0.005)
  cn[names(control)] <- control
  more <- list(...)[c("nstart", "trace", "lower", "opt", "rotate")]
  if (length(more)) 
    cn[names(more)] <- more
  if (is.null(start)) {
    start <- (1 - 0.5 * factors/p)/diag(solve(cv))
    if ((ns <- cn$nstart) > 1) 
      start <- cbind(start, matrix(runif(ns - 1), p, ns - 1, byrow = TRUE))
  }
  start <- as.matrix(start)
  if (nrow(start) != p) 
    stop(sprintf(ngettext(p, "'start' must have %d row", 
                          "'start' must have %d rows"), p), domain = NA)
  nc <- ncol(start)
  if (nc < 1) 
    stop("no starting values supplied")
  best <- Inf
  for (i in 1L:nc) {
    print(dput(cv))
    print(dput(start[, i]))
    print(list(
      factors = factors, lower = max(cn$lower, 0), control = cn$opt
    ))
    nfit <- stats:::factanal.fit.mle(cv, factors, start[, i],
                                     max(cn$lower, 0), cn$opt)
    if (cn$trace) 
      cat("start", i, "value:", format(nfit$criteria[1L]), 
          "uniqs:", format(as.vector(round(nfit$uniquenesses, 4))), "\n")
    if (nfit$converged && nfit$criteria[1L] < best) {
      fit <- nfit
      best <- fit$criteria[1L]
    }
  }
  if (best == Inf) 
    stop(ngettext(nc, "unable to optimize from this starting value", 
                  "unable to optimize from these starting values"), 
         domain = NA)
  load <- fit$loadings
  if (rotation != "none") {
    rot <- do.call(rotation, c(list(load), cn$rotate))
    load <- if (is.list(rot)) {
      load <- rot$loadings
      fit$rotmat <- if (inherits(rot, "GPArotation")) 
        t(solve(rot$Th))
      else rot$rotmat
      rot$loadings
    } else rot
  }
  fit$loadings <- sortLoadings(load)
  class(fit$loadings) <- "loadings"
  fit$na.action <- na.act
  if (have.x && scores != "none") {
    Lambda <- fit$loadings
    zz <- scale(z, TRUE, TRUE)
    switch(scores, regression = {
      sc <- zz %*% solve(cv, Lambda)
      if (!is.null(Phi <- attr(Lambda, "covariance"))) sc <- sc %*% Phi
    }, Bartlett = {
      d <- 1/fit$uniquenesses
      tmp <- t(Lambda * d)
      sc <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
    })
    rownames(sc) <- rownames(z)
    colnames(sc) <- colnames(Lambda)
    if (!is.null(na.act)) 
      sc <- napredict(na.act, sc)
    fit$scores <- sc
  }
  if (!is.na(n.obs) && dof > 0) {
    fit$STATISTIC <- (n.obs - 1 - (2 * p + 5)/6 - (2 * factors)/3) * 
      fit$criteria["objective"]
    fit$PVAL <- pchisq(fit$STATISTIC, dof, lower.tail = FALSE)
  }
  fit$n.obs <- n.obs
  fit$call <- cl
  fit
}

fa <- factanal_visible(
  ~ ., factors = 2L, data = swiss,
  scores = "regression", rotation = "none"
)
# reconstruction
fa$correlation - (fa$loadings %*% t(fa$loadings) + diag(fa$uniquenesses))

# StackExchange example
# https://stackoverflow.com/q/25337134/4556798

data(bfi, package = "psych")
bfi %>%
  dplyr::slice(seq(120L)) %>%
  dplyr::select(16:25, gender, education, age) %>%
  dplyr::filter(dplyr::if_all(1:10, ~ ! is.na(.))) ->
  bfi_sub
X <- dplyr::select(bfi_sub, -gender, -education, -age)
Z <- dplyr::select(bfi_sub, gender, education, age)

fa1 <- factanal(X, 2L, scores = "regression", rotation = "none")

# calculate scores
x <- X
z <- as.matrix(x)
covmat <- cov.wt(z)
cv <- covmat$cov
sds <- sqrt(diag(cv))
cv <- cv/(sds %o% sds)
load <- unclass(fa1$loadings)
Lambda <- load
zz <- scale(z, TRUE, TRUE)

# regression
sc_reg <- zz %*% solve(cv, Lambda)
head(sc_reg)
# Bartlett
d <- 1/fa1$uniquenesses
tmp <- t(Lambda * d)
sc_bart <- t(solve(tmp %*% Lambda, tmp %*% t(zz)))
head(sc_bart)

# regression, shorter
x_cov <- cov.wt(x)$cov
head(
  scale(x) %*%
    solve(x_cov / (sqrt(diag(x_cov)) %o% sqrt(diag(x_cov))), fa1$loadings)
)
head(scale(x) %*% solve(fa1$correlation, fa1$loadings))
head(fa1$scores)

# Bartlett, shorter
tmp <- t(fa1$loadings * 1/fa1$uniquenesses)
head(t(solve(tmp %*% fa1$loadings, tmp %*% t(scale(x)))))
# scores * inner product of loadings * twice-de-scaled loadings
head(fa1$scores %*% t(fa1$loadings) %*% (fa1$loadings * 1/fa1$uniquenesses))
# scores * inner products of de-scaled loadings
head(
  fa1$scores %*%
    t(fa1$loadings / sqrt(fa1$uniquenesses)) %*%
    (fa1$loadings / sqrt(fa1$uniquenesses))
)
# scaled data * twice-de-scaled loadings
head(scale(x) %*% (fa1$loadings * 1/fa1$uniquenesses))

# reconstruct data? no
# interpretation depends on score type; regression scores are coefficients
# how to confer inertia?
load_std <- sweep(
  unclass(fa1$loadings), 2,
  apply(fa1$loadings, 2, norm, "2"), "/"
)
head(scale(x) %*% solve(fa1$correlation, load_std))
head(fa1$scores) %*% diag(1 / apply(fa1$loadings, 2, norm, "2"))
# yes, inertia can be handled as in SVD biplots;
# scale scores out while scaling loadings in;
# but...scores initially have no inertia? (b/c data is scaled)

cmat <- cv
start <- (1 - 0.5 * 2L/ncol(cv))/diag(solve(cv))


fa1 %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  ggbiplot(sec.axes = "rows", scale.factor = 1/3) +
  geom_rows_point(elements = "supp") +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  geom_cols_vector(arrow = NULL) +
  geom_cols_text(aes(label = .name))

fa1 %>%
  as_tbl_ord() %>%
  augment_ord() %>%
  confer_inertia("rows") %>%
  # -+- need syntax to bind to only primary or supplementary points -+-
  cbind_rows(dplyr::bind_rows(ordr:::tibble_pole(10L), Z)) %>%
  ggbiplot() +
  geom_rows_point(aes(shape = factor(gender), color = age)) +
  scale_alpha_manual(values = c(0, 1), guide = "none") +
  geom_unit_circle() +
  geom_cols_vector(arrow = NULL) +
  geom_cols_text(aes(label = .name))
