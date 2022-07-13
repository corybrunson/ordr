# `psych::factor.scores()`
function (
    x, f, Phi = NULL,
    method = c("Thurstone", "tenBerge", 
               "Anderson", "Bartlett", "Harman", "components"),
    rho = NULL, 
    impute = "none"
) {
  if (length(method) > 1) 
    method <- "tenBerge"
  if (method == "regression") 
    method <- "Thurstone"
  if (method %in% c("tenberge", "Tenberge", "tenBerge", "TenBerge")) 
    method <- "tenBerge"
  if (length(class(f)) > 1) {
    if (inherits(f[2], "irt.fa")) 
      f <- f$fa
  }
  if (!is.matrix(f)) {
    Phi <- f$Phi
    f <- loadings(f)
    if (ncol(f) == 1) {
      method <- "Thurstone"
    }
  }
  nf <- dim(f)[2]
  if (is.null(Phi)) 
    Phi <- diag(1, nf, nf)
  if (dim(x)[1] == dim(f)[1]) {
    r <- as.matrix(x)
    square <- TRUE
  }
  else {
    square <- FALSE
    if (!is.null(rho)) {
      r <- rho
    }
    else {
      r <- cor(x, use = "pairwise")
    }
  }
  S <- f %*% Phi
  switch(method, Thurstone = {
    w <- try(solve(r, S), silent = TRUE)
    if (inherits(w, "try-error")) {
      message("In factor.scores, the correlation matrix is singular, an approximation is used")
      r <- cor.smooth(r)
    }
    w <- try(solve(r, S), silent = TRUE)
    if (inherits(w, "try-error")) {
      message("I was unable to calculate the factor score weights, factor loadings used instead")
      w <- f
    }
    colnames(w) <- colnames(f)
    rownames(w) <- rownames(f)
  }, tenBerge = {
    L <- f %*% matSqrt(Phi)
    r.5 <- invMatSqrt(r)
    r <- cor.smooth(r)
    inv.r <- try(solve(r), silent = TRUE)
    if (inherits(inv.r, as.character("try-error"))) {
      warning("The tenBerge based scoring could not invert the correlation matrix, regression scores found instead")
      ev <- eigen(r)
      ev$values[ev$values < .Machine$double.eps] <- 100 * 
        .Machine$double.eps
      r <- ev$vectors %*% diag(ev$values) %*% t(ev$vectors)
      diag(r) <- 1
      w <- solve(r, f)
    } else {
      C <- r.5 %*% L %*% invMatSqrt(t(L) %*% inv.r %*% 
                                      L)
      w <- r.5 %*% C %*% matSqrt(Phi)
    }
    colnames(w) <- colnames(f)
    rownames(w) <- rownames(f)
  }, Harman = {
    m <- f %*% t(S)
    diag(m) <- 1
    inv.m <- solve(m)
    w <- inv.m %*% f
  }, Anderson = {
    I <- diag(1, nf, nf)
    h2 <- diag(f %*% Phi %*% t(f))
    U2 <- 1 - h2
    inv.U2 <- diag(1/U2)
    w <- inv.U2 %*% f %*% invMatSqrt(t(f) %*% inv.U2 %*% 
                                       r %*% inv.U2 %*% f)
    colnames(w) <- colnames(f)
    rownames(w) <- rownames(f)
  }, Bartlett = {
    I <- diag(1, nf, nf)
    h2 <- diag(f %*% Phi %*% t(f))
    U2 <- 1 - h2
    inv.U2 <- diag(1/U2)
    w <- inv.U2 %*% f %*% (solve(t(f) %*% inv.U2 %*% f))
    colnames(w) <- colnames(f)
    rownames(w) <- rownames(f)
  }, none = {
    w <- NULL
  }, components = {
    w <- try(solve(r, f), silent = TRUE)
    w <- f
  })
  if (is.null(w)) {
    results <- list(scores = NULL, weights = NULL)
  }
  else {
    R2 <- diag(t(w) %*% S)
    if (any(R2 > 1) || (prod(!is.nan(R2)) < 1) || (prod(R2) < 
                                                   0)) {
      R2[abs(R2) > 1] <- NA
      R2[R2 <= 0] <- NA
    }
    r.scores <- cov2cor(t(w) %*% r %*% w)
    if (square) {
      class(w) <- NULL
      results <- list(scores = NULL, weights = w)
      results$r.scores <- r.scores
      results$R2 <- R2
    }
    else {
      missing <- rowSums(is.na(x))
      if (impute != "none") {
        x <- data.matrix(x)
        miss <- which(is.na(x), arr.ind = TRUE)
        if (impute == "mean") {
          item.means <- colMeans(x, na.rm = TRUE)
          x[miss] <- item.means[miss[, 2]]
        }
        else {
          item.med <- apply(x, 2, median, na.rm = TRUE)
          x[miss] <- item.med[miss[, 2]]
        }
      }
      if (method != "components") {
        scores <- scale(x) %*% w
      }
      else {
        scores <- x %*% w
      }
      results <- list(scores = scores, weights = w)
      results$r.scores <- r.scores
      results$missing <- missing
      results$R2 <- R2
    }
  }
  return(results)
}

# `psych::fac()`
function (
    r, nfactors = 1, n.obs = NA, rotate = "oblimin", scores = "tenBerge", 
    residuals = FALSE, SMC = TRUE, covar = FALSE, missing = FALSE, 
    impute = "median", min.err = 0.001, max.iter = 50, symmetric = TRUE, 
    warnings = TRUE, fm = "minres", alpha = 0.1, oblique.scores = FALSE, 
    np.obs = NULL, use = "pairwise", cor = "cor", correct = 0.5, 
    weight = NULL, n.rotations = 1, hyper = 0.15, ...
) {
  cl <- match.call()
  control <- NULL
  "fit.residuals" <- function(Psi, S, nf, S.inv = NULL, fm) {
    diag(S) <- 1 - Psi
    if (!is.null(S.inv)) 
      sd.inv <- diag(1/diag(S.inv))
    eigens <- eigen(S)
    eigens$values[eigens$values < .Machine$double.eps] <- 100 * 
      .Machine$double.eps
    if (nf > 1) {
      loadings <- eigens$vectors[, 1:nf] %*% diag(sqrt(eigens$values[1:nf]))
    }
    else {
      loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
    }
    model <- loadings %*% t(loadings)
    switch(fm, wls = {
      residual <- sd.inv %*% (S - model)^2 %*% sd.inv
    }, gls = {
      residual <- (S.inv %*% (S - model))^2
    }, uls = {
      residual <- (S - model)^2
    }, ols = {
      residual <- (S - model)
      residual <- residual[lower.tri(residual)]
      residual <- residual^2
    }, minres = {
      residual <- (S - model)
      residual <- residual[lower.tri(residual)]
      residual <- residual^2
    }, old.min = {
      residual <- (S - model)
      residual <- residual[lower.tri(residual)]
      residual <- residual^2
    }, minchi = {
      residual <- (S - model)^2
      residual <- residual * np.obs
      diag(residual) <- 0
    })
    error <- sum(residual)
  }
  "fit" <- function(S, nf, fm, covar) {
    if (is.logical(SMC)) {
      S.smc <- smc(S, covar)
    }
    else {
      S.smc <- SMC
    }
    upper <- max(S.smc, 1)
    if ((fm == "wls") | (fm == "gls")) {
      S.inv <- solve(S)
    }
    else {
      S.inv <- NULL
    }
    if (!covar && (sum(S.smc) == nf) && (nf > 1)) {
      start <- rep(0.5, nf)
    }
    else {
      start <- diag(S) - S.smc
    }
    if (fm == "ml" || fm == "mle") {
      res <- optim(start, FAfn, FAgr, method = "L-BFGS-B", 
                   lower = 0.005, upper = upper, control = c(list(fnscale = 1, 
                                                                  parscale = rep(0.01, length(start))), control), 
                   nf = nf, S = S)
    }
    else {
      if (fm == "ols") {
        if (is.logical(SMC)) {
          start <- diag(S) - smc(S, covar)
        }
        else {
          start <- SMC
        }
        res <- optim(start, FA.OLS, method = "L-BFGS-B", 
                     lower = 0.005, upper = upper, control = c(list(fnscale = 1, 
                                                                    parscale = rep(0.01, length(start)))), nf = nf, 
                     S = S)
      }
      else {
        if ((fm == "minres") | (fm == "uls")) {
          start <- diag(S) - smc(S, covar)
          res <- optim(start, fit.residuals, gr = FAgr.minres, 
                       method = "L-BFGS-B", lower = 0.005, upper = upper, 
                       control = c(list(fnscale = 1, parscale = rep(0.01, 
                                                                    length(start)))), nf = nf, S = S, fm = fm)
        }
        else {
          start <- smc(S, covar)
          res <- optim(start, fit.residuals, gr = FAgr.minres2, 
                       method = "L-BFGS-B", lower = 0.005, upper = upper, 
                       control = c(list(fnscale = 1, parscale = rep(0.01, 
                                                                    length(start)))), nf = nf, S = S, S.inv = S.inv, 
                       fm = fm)
        }
      }
    }
    if ((fm == "wls") | (fm == "gls") | (fm == "ols") | (fm == 
                                                         "uls") | (fm == "minres") | (fm == "old.min")) {
      Lambda <- FAout.wls(res$par, S, nf)
    }
    else {
      Lambda <- FAout(res$par, S, nf)
    }
    result <- list(loadings = Lambda, res = res, S = S)
  }
  FAfn <- function(Psi, S, nf) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE, only.values = TRUE)
    e <- E$values[-(1:nf)]
    e <- sum(log(e) - e) - nf + nrow(S)
    -e
  }
  FAgr <- function(Psi, S, nf) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1:nf, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), 
                       nf)
    load <- diag(sqrt(Psi)) %*% load
    g <- load %*% t(load) + diag(Psi) - S
    diag(g)/Psi^2
  }
  FAgr.minres2 <- function(Psi, S, nf, S.inv, fm) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1:nf, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1:nf] - 1, 0)), 
                       nf)
    load <- diag(sqrt(Psi)) %*% load
    g <- load %*% t(load) + diag(Psi) - S
    if (fm == "minchi") {
      g <- g * np.obs
    }
    diag(g)/Psi^2
  }
  FAgr.minres <- function(Psi, S, nf, fm) {
    Sstar <- S - diag(Psi)
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1:nf, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1:nf], 0)), nf)
    g <- load %*% t(load) + diag(Psi) - S
    diag(g)
  }
  FAout <- function(Psi, S, q) {
    sc <- diag(1/sqrt(Psi))
    Sstar <- sc %*% S %*% sc
    E <- eigen(Sstar, symmetric = TRUE)
    L <- E$vectors[, 1L:q, drop = FALSE]
    load <- L %*% diag(sqrt(pmax(E$values[1L:q] - 1, 0)), 
                       q)
    diag(sqrt(Psi)) %*% load
  }
  FAout.wls <- function(Psi, S, q) {
    diag(S) <- diag(S) - Psi
    E <- eigen(S, symmetric = TRUE)
    L <- E$vectors[, 1L:q, drop = FALSE] %*% diag(sqrt(pmax(E$values[1L:q, 
                                                                     drop = FALSE], 0)), q)
    return(L)
  }
  "MRFA" <- function(S, nf) {
    com.glb <- glb.algebraic(S)
    L <- FAout.wls(1 - com.glb$solution, S, nf)
    h2 <- com.glb$solution
    result <- list(loadings = L, communality = h2)
  }
  FA.OLS <- function(Psi, S, nf) {
    E <- eigen(S - diag(Psi), symmetric = T)
    U <- E$vectors[, 1:nf, drop = FALSE]
    D <- E$values[1:nf, drop = FALSE]
    D[D < 0] <- 0
    if (length(D) < 2) {
      L <- U * sqrt(D)
    }
    else {
      L <- U %*% diag(sqrt(D))
    }
    model <- L %*% t(L)
    diag(model) <- diag(S)
    return(sum((S - model)^2)/2)
  }
  FAgr.OLS <- function(Psi, S, nf) {
    E <- eigen(S - diag(Psi), symmetric = TRUE)
    U <- E$vectors[, 1:nf, drop = FALSE]
    D <- E$values[1:nf]
    D[D < 0] <- 0
    L <- U %*% diag(sqrt(D))
    model <- L %*% t(L)
    g <- diag(Psi) - diag(S - model)
    diag(g)/Psi^2
  }
  if (fm == "mle" || fm == "MLE" || fm == "ML") 
    fm <- "ml"
  if (!any(fm %in% (c("pa", "alpha", "minrank", "wls", "gls", 
                      "minres", "minchi", "uls", "ml", "mle", "ols", "old.min")))) {
    message("factor method not specified correctly, minimum residual (unweighted least squares  used")
    fm <- "minres"
  }
  x.matrix <- r
  n <- dim(r)[2]
  if (!isCorrelation(r) & !isCovariance(r)) {
    matrix.input <- FALSE
    n.obs <- dim(r)[1]
    if (missing) {
      x.matrix <- as.matrix(x.matrix)
      miss <- which(is.na(x.matrix), arr.ind = TRUE)
      if (impute == "mean") {
        item.means <- colMeans(x.matrix, na.rm = TRUE)
        x.matrix[miss] <- item.means[miss[, 2]]
      }
      else {
        item.med <- apply(x.matrix, 2, median, na.rm = TRUE)
        x.matrix[miss] <- item.med[miss[, 2]]
      }
    }
    np.obs <- pairwiseCount(r)
    if (covar) {
      cor <- "cov"
    }
    switch(cor, cor = {
      if (!is.null(weight)) {
        r <- cor.wt(r, w = weight)$r
      } else {
        r <- cor(r, use = use)
      }
    }, cov = {
      r <- cov(r, use = use)
      covar <- TRUE
    }, wtd = {
      r <- cor.wt(r, w = weight)$r
    }, spearman = {
      r <- cor(r, use = use, method = "spearman")
    }, kendall = {
      r <- cor(r, use = use, method = "kendall")
    }, tet = {
      r <- tetrachoric(r, correct = correct, weight = weight)$rho
    }, poly = {
      r <- polychoric(r, correct = correct, weight = weight)$rho
    }, tetrachoric = {
      r <- tetrachoric(r, correct = correct, weight = weight)$rho
    }, polychoric = {
      r <- polychoric(r, correct = correct, weight = weight)$rho
    }, mixed = {
      r <- mixedCor(r, use = use, correct = correct)$rho
    }, Yuleb = {
      r <- YuleCor(r, , bonett = TRUE)$rho
    }, YuleQ = {
      r <- YuleCor(r, 1)$rho
    }, YuleY = {
      r <- YuleCor(r, 0.5)$rho
    })
  }
  else {
    matrix.input <- TRUE
    if (fm == "minchi") {
      if (is.null(np.obs)) {
        fm <- "minres"
        message("factor method minchi does not make sense unless we know the sample size, minres used instead")
      }
    }
    if (is.na(n.obs) && !is.null(np.obs)) 
      n.obs <- max(as.vector(np.obs))
    if (!is.matrix(r)) {
      r <- as.matrix(r)
    }
    if (!covar) {
      r <- cov2cor(r)
    }
  }
  if (!residuals) {
    result <- list(values = c(rep(0, n)), rotation = rotate, 
                   n.obs = n.obs, np.obs = np.obs, communality = c(rep(0, 
                                                                       n)), loadings = matrix(rep(0, n * n), ncol = n), 
                   fit = 0)
  }
  else {
    result <- list(values = c(rep(0, n)), rotation = rotate, 
                   n.obs = n.obs, np.obs = np.obs, communality = c(rep(0, 
                                                                       n)), loadings = matrix(rep(0, n * n), ncol = n), 
                   residual = matrix(rep(0, n * n), ncol = n), fit = 0, 
                   r = r)
  }
  if (is.null(SMC)) 
    SMC = TRUE
  r.mat <- r
  Phi <- NULL
  colnames(r.mat) <- rownames(r.mat) <- colnames(r)
  if (any(is.na(r))) {
    bad <- TRUE
    tempr <- r
    wcl <- NULL
    while (bad) {
      wc <- table(which(is.na(tempr), arr.ind = TRUE))
      wcl <- c(wcl, as.numeric(names(which(wc == max(wc)))))
      tempr <- r[-wcl, -wcl]
      if (any(is.na(tempr))) {
        bad <- TRUE
      }
      else {
        bad <- FALSE
      }
    }
    cat("\nLikely variables with missing values are ", colnames(r)[wcl], 
        " \n")
    stop("I am sorry: missing values (NAs) in the correlation matrix do not allow me to continue.\nPlease drop those variables and try again.")
  }
  if (is.logical(SMC)) {
    if (SMC) {
      if (nfactors <= n) {
        diag(r.mat) <- smc(r, covar = covar)
      }
      else {
        if (warnings) {
          message("In fa, too many factors requested for this number of variables to use SMC for communality estimates, 1s are used instead")
        }
      }
    }
    else {
      diag(r.mat) <- 1
    }
  }
  else {
    diag(r.mat) <- SMC
  }
  orig <- diag(r)
  comm <- sum(diag(r.mat))
  err <- comm
  i <- 1
  comm.list <- list()
  if (fm == "alpha") {
    i <- 1
    e.values <- eigen(r, symmetric = symmetric)$values
    H2 <- diag(r.mat)
    while (err > min.err) {
      r.mat <- cov2cor(r.mat)
      eigens <- eigen(r.mat, symmetric = symmetric)
      loadings <- eigens$vectors[, 1:nfactors, drop = FALSE] %*% 
        diag(sqrt(eigens$values[1:nfactors, drop = FALSE]))
      model <- loadings %*% t(loadings)
      newH2 <- H2 * diag(model)
      err <- sum(abs(H2 - newH2))
      r.mat <- r
      diag(r.mat) <- newH2
      H2 <- newH2
      i <- i + 1
      if (i > max.iter) {
        if (warnings) {
          message("maximum iteration exceeded")
        }
        err <- 0
      }
    }
    loadings <- sqrt(H2) * loadings
    eigens <- sqrt(H2) * eigens$vaues
    comm1 <- sum(H2)
  }
  if (fm == "pa") {
    e.values <- eigen(r, symmetric = symmetric)$values
    while (err > min.err) {
      eigens <- eigen(r.mat, symmetric = symmetric)
      if (nfactors > 1) {
        loadings <- eigens$vectors[, 1:nfactors] %*% 
          diag(sqrt(eigens$values[1:nfactors]))
      }
      else {
        loadings <- eigens$vectors[, 1] * sqrt(eigens$values[1])
      }
      model <- loadings %*% t(loadings)
      new <- diag(model)
      comm1 <- sum(new)
      diag(r.mat) <- new
      err <- abs(comm - comm1)
      if (is.na(err)) {
        warning("imaginary eigen value condition encountered in fa\n Try again with SMC=FALSE \n exiting fa")
        break
      }
      comm <- comm1
      comm.list[[i]] <- comm1
      i <- i + 1
      if (i > max.iter) {
        if (warnings) {
          message("maximum iteration exceeded")
        }
        err <- 0
      }
    }
    eigens <- eigens$values
  }
  if (fm == "minrank") {
    mrfa <- MRFA(r, nfactors)
    loadings <- mrfa$loadings
    model <- loadings %*% t(loadings)
    e.values <- eigen(r)$values
    S <- r
    diag(S) <- diag(model)
    eigens <- eigen(S)$values
  }
  if ((fm == "wls") | (fm == "minres") | (fm == "minchi") | 
      (fm == "gls") | (fm == "uls") | (fm == "ml") | (fm == 
                                                      "mle") | (fm == "ols") | (fm == "old.min")) {
    uls <- fit(r, nfactors, fm, covar = covar)
    e.values <- eigen(r)$values
    result.res <- uls$res
    loadings <- uls$loadings
    model <- loadings %*% t(loadings)
    S <- r
    diag(S) <- diag(model)
    eigens <- eigen(S)$values
  }
  if (!is.double(loadings)) {
    warning("the matrix has produced imaginary results -- proceed with caution")
    loadings <- matrix(as.double(loadings), ncol = nfactors)
  }
  if (nfactors > 1) {
    sign.tot <- vector(mode = "numeric", length = nfactors)
    sign.tot <- sign(colSums(loadings))
    sign.tot[sign.tot == 0] <- 1
    loadings <- loadings %*% diag(sign.tot)
  }
  else {
    if (sum(loadings) < 0) {
      loadings <- -as.matrix(loadings)
    }
    else {
      loadings <- as.matrix(loadings)
    }
    colnames(loadings) <- "MR1"
  }
  switch(fm, alpha = {
    colnames(loadings) <- paste("alpha", 1:nfactors, sep = "")
  }, wls = {
    colnames(loadings) <- paste("WLS", 1:nfactors, sep = "")
  }, pa = {
    colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
  }, gls = {
    colnames(loadings) <- paste("GLS", 1:nfactors, sep = "")
  }, ml = {
    colnames(loadings) <- paste("ML", 1:nfactors, sep = "")
  }, minres = {
    colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
  }, minrank = {
    colnames(loadings) <- paste("MRFA", 1:nfactors, sep = "")
  }, minchi = {
    colnames(loadings) <- paste("MC", 1:nfactors, sep = "")
  })
  rownames(loadings) <- rownames(r)
  loadings[loadings == 0] <- 10^-15
  model <- loadings %*% t(loadings)
  f.loadings <- loadings
  rot.mat <- NULL
  rotated <- NULL
  if (rotate != "none") {
    if (nfactors > 1) {
      if (n.rotations > 1) {
        rotated <- faRotations(loadings, r = r, n.rotations = n.rotations, 
                               rotate = rotate, hyper = hyper)
        loadings = rotated$loadings
        Phi <- rotated$Phi
        rot.mat = rotated$rot.mat
      }
      else {
        rotated <- NULL
        if (rotate == "varimax" | rotate == "Varimax" | 
            rotate == "quartimax" | rotate == "bentlerT" | 
            rotate == "geominT" | rotate == "targetT" | 
            rotate == "bifactor" | rotate == "TargetT" | 
            rotate == "equamax" | rotate == "varimin" | 
            rotate == "specialT" | rotate == "Promax" | 
            rotate == "promax" | rotate == "cluster" | 
            rotate == "biquartimin" | rotate == "TargetQ" | 
            rotate == "specialQ") {
          Phi <- NULL
          switch(rotate, varimax = {
            rotated <- stats::varimax(loadings)
            loadings <- rotated$loadings
            rot.mat <- rotated$rotmat
          }, Varimax = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rotated <- GPArotation::Varimax(loadings, 
                                            ...)
            loadings <- rotated$loadings
            rot.mat <- t(solve(rotated$Th))
          }, quartimax = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rotated <- GPArotation::quartimax(loadings, 
                                              ...)
            loadings <- rotated$loadings
            rot.mat <- t(solve(rotated$Th))
          }, bentlerT = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rotated <- GPArotation::bentlerT(loadings, 
                                             ...)
            loadings <- rotated$loadings
            rot.mat <- t(solve(rotated$Th))
          }, geominT = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rotated <- GPArotation::geominT(loadings, 
                                            ...)
            loadings <- rotated$loadings
            rot.mat <- t(solve(rotated$Th))
          }, targetT = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rotated <- GPArotation::targetT(loadings, 
                                            Tmat = diag(ncol(loadings)), ...)
            loadings <- rotated$loadings
            rot.mat <- t(solve(rotated$Th))
          }, bifactor = {
            rot <- bifactor(loadings, ...)
            loadings <- rot$loadings
            rot.mat <- t(solve(rot$Th))
          }, TargetT = {
            if (!requireNamespace("GPArotation")) {
              stop("I am sorry, to do this rotation requires the GPArotation package to be installed")
            }
            rot <- GPArotation::targetT(loadings, Tmat = diag(ncol(loadings)), 
                                        ...)
            loadings <- rot$loadings
            rot.mat <- t(solve(rot$Th))
          }, equamax = {
            rot <- equamax(loadings, ...)
            loadings <- rot$loadings
            rot.mat <- t(solve(rot$Th))
          }, varimin = {
            rot <- varimin(loadings, ...)
            loadings <- rot$loadings
            rot.mat <- t(solve(rot$Th))
          }, specialT = {
            rot <- specialT(loadings, ...)
            loadings <- rot$loadings
            rot.mat <- t(solve(rot$Th))
          }, Promax = {
            pro <- Promax(loadings, ...)
            loadings <- pro$loadings
            Phi <- pro$Phi
            rot.mat <- pro$rotmat
          }, promax = {
            pro <- kaiser(loadings, rotate = "Promax", 
                          ...)
            loadings <- pro$loadings
            rot.mat <- pro$rotmat
            Phi <- pro$Phi
          }, cluster = {
            loadings <- varimax(loadings, ...)$loadings
            pro <- target.rot(loadings)
            loadings <- pro$loadings
            Phi <- pro$Phi
            rot.mat <- pro$rotmat
          }, biquartimin = {
            ob <- biquartimin(loadings, ...)
            loadings <- ob$loadings
            Phi <- ob$Phi
            rot.mat <- t(solve(ob$Th))
          }, TargetQ = {
            ob <- TargetQ(loadings, ...)
            loadings <- ob$loadings
            Phi <- ob$Phi
            rot.mat <- t(solve(ob$Th))
          }, specialQ = {
            ob <- specialQ(loadings, ...)
            loadings <- ob$loadings
            Phi <- ob$Phi
            rot.mat <- t(solve(pro$Th))
          })
        }
        else {
          if (rotate == "oblimin" | rotate == "quartimin" | 
              rotate == "simplimax" | rotate == "geominQ" | 
              rotate == "bentlerQ" | rotate == "targetQ") {
            if (!requireNamespace("GPArotation")) {
              warning("I am sorry, to do these rotations requires the GPArotation package to be installed")
              Phi <- NULL
            }
            else {
              ob <- try(do.call(getFromNamespace(rotate, 
                                                 "GPArotation"), list(loadings, ...)))
              if (inherits(ob, as.character("try-error"))) {
                warning("The requested transformaton failed, Promax was used instead as an oblique transformation")
                ob <- Promax(loadings)
              }
              loadings <- ob$loadings
              Phi <- ob$Phi
              rot.mat <- t(solve(ob$Th))
            }
          }
          else {
            message("Specified rotation not found, rotate='none' used")
          }
        }
      }
    }
  }
  else {
    rotated <- NULL
  }
  signed <- sign(colSums(loadings))
  signed[signed == 0] <- 1
  loadings <- loadings %*% diag(signed)
  if (!is.null(Phi)) {
    Phi <- diag(signed) %*% Phi %*% diag(signed)
  }
  switch(fm, alpha = {
    colnames(loadings) <- paste("alpha", 1:nfactors, sep = "")
  }, wls = {
    colnames(loadings) <- paste("WLS", 1:nfactors, sep = "")
  }, pa = {
    colnames(loadings) <- paste("PA", 1:nfactors, sep = "")
  }, gls = {
    colnames(loadings) <- paste("GLS", 1:nfactors, sep = "")
  }, ml = {
    colnames(loadings) <- paste("ML", 1:nfactors, sep = "")
  }, minres = {
    colnames(loadings) <- paste("MR", 1:nfactors, sep = "")
  }, minrank = {
    colnames(loadings) <- paste("MRFA", 1:nfactors, sep = "")
  }, uls = {
    colnames(loadings) <- paste("ULS", 1:nfactors, sep = "")
  }, old.min = {
    colnames(loadings) <- paste0("oldmin", 1:nfactors)
  }, minchi = {
    colnames(loadings) <- paste("MC", 1:nfactors, sep = "")
  })
  if (nfactors > 1) {
    if (is.null(Phi)) {
      ev.rotated <- diag(t(loadings) %*% loadings)
    }
    else {
      ev.rotated <- diag(Phi %*% t(loadings) %*% loadings)
    }
    ev.order <- order(ev.rotated, decreasing = TRUE)
    loadings <- loadings[, ev.order]
  }
  rownames(loadings) <- colnames(r)
  if (!is.null(Phi)) {
    Phi <- Phi[ev.order, ev.order]
  }
  class(loadings) <- "loadings"
  if (nfactors < 1) 
    nfactors <- n
  result <- factor.stats(r, loadings, Phi, n.obs = n.obs, np.obs = np.obs, 
                         alpha = alpha)
  result$rotation <- rotate
  if (nfactors != 1) {
    result$hyperplane <- colSums(abs(loadings) < hyper)
  }
  else {
    result$hyperplane <- sum(abs(loadings) < hyper)
  }
  result$communality <- diag(model)
  if (max(result$communality > 1) && !covar) 
    warning("An ultra-Heywood case was detected.  Examine the results carefully")
  if (fm == "minrank") {
    result$communalities <- mrfa$communality
  }
  else {
    if (fm == "pa" | fm == "alpha") {
      result$communalities <- comm1
    }
    else {
      result$communalities <- 1 - result.res$par
    }
  }
  result$uniquenesses <- diag(r - model)
  result$values <- eigens
  result$e.values <- e.values
  result$loadings <- loadings
  result$model <- model
  result$fm <- fm
  result$rot.mat <- rot.mat
  if (!is.null(Phi)) {
    colnames(Phi) <- rownames(Phi) <- colnames(loadings)
    result$Phi <- Phi
    Structure <- loadings %*% Phi
  }
  else {
    Structure <- loadings
  }
  class(Structure) <- "loadings"
  result$Structure <- Structure
  if (fm == "pa") 
    result$communality.iterations <- unlist(comm.list)
  result$method = scores
  if (oblique.scores) {
    result$scores <- factor.scores(x.matrix, f = loadings, 
                                   Phi = NULL, method = scores)
  }
  else {
    result$scores <- factor.scores(x.matrix, f = loadings, 
                                   Phi = Phi, method = scores)
  }
  if (is.null(result$scores$R2)) 
    result$scores$R2 <- NA
  result$R2.scores <- result$scores$R2
  result$weights <- result$scores$weights
  result$scores <- result$scores$scores
  if (!is.null(result$scores)) 
    colnames(result$scores) <- colnames(loadings)
  result$factors <- nfactors
  result$r <- r
  result$np.obs <- np.obs
  result$fn <- "fa"
  result$fm <- fm
  if (is.null(Phi)) {
    if (nfactors > 1) {
      vx <- colSums(loadings^2)
    }
    else {
      vx <- sum(loadings^2)
    }
  }
  else {
    vx <- diag(Phi %*% t(loadings) %*% loadings)
  }
  vtotal <- sum(result$communality + result$uniquenesses)
  names(vx) <- colnames(loadings)
  varex <- rbind(`SS loadings` = vx)
  varex <- rbind(varex, `Proportion Var` = vx/vtotal)
  if (nfactors > 1) {
    varex <- rbind(varex, `Cumulative Var` = cumsum(vx/vtotal))
    varex <- rbind(varex, `Proportion Explained` = vx/sum(vx))
    varex <- rbind(varex, `Cumulative Proportion` = cumsum(vx/sum(vx)))
  }
  result$rotated <- rotated$rotation.stats
  result$Vaccounted <- varex
  result$Call <- cl
  class(result) <- c("psych", "fa")
  return(result)
}

# `psych::fa()`
function (
    r, nfactors = 1, n.obs = NA, n.iter = 1, rotate = "oblimin", 
    scores = "regression", residuals = FALSE, SMC = TRUE, covar = FALSE, 
    missing = FALSE, impute = "median", min.err = 0.001, max.iter = 50, 
    symmetric = TRUE, warnings = TRUE, fm = "minres", alpha = 0.1, 
    p = 0.05, oblique.scores = FALSE, np.obs = NULL, use = "pairwise", 
    cor = "cor", correct = 0.5, weight = NULL, n.rotations = 1, 
    hyper = 0.15, ...
) {
  cl <- match.call()
  if (isCorrelation(r)) {
    if (is.na(n.obs) && (n.iter > 1)) 
      stop("You must specify the number of subjects if giving a correlation matrix and doing confidence intervals")
    if (length(class(r)) > 1) {
      if (inherits(r, "partial.r")) 
        class(r) <- c("matrix", "array")
    }
  }
  f <- fac(r = r, nfactors = nfactors, n.obs = n.obs, rotate = rotate, 
           scores = scores, residuals = residuals, SMC = SMC, covar = covar, 
           missing = missing, impute = impute, min.err = min.err, 
           max.iter = max.iter, symmetric = symmetric, warnings = warnings, 
           fm = fm, alpha = alpha, oblique.scores = oblique.scores, 
           np.obs = np.obs, use = use, cor = cor, correct = correct, 
           weight = weight, n.rotations = n.rotations, hyper = hyper, 
           ... = ...)
  fl <- f$loadings
  nvar <- dim(fl)[1]
  if (n.iter > 1) {
    if (is.na(n.obs)) {
      n.obs <- f$n.obs
    }
    replicates <- list()
    rep.rots <- list()
    replicateslist <- parallel::mclapply(1:n.iter, function(x) {
      if (isCorrelation(r)) {
        mu <- rep(0, nvar)
        eX <- eigen(r)
        X <- matrix(rnorm(nvar * n.obs), n.obs)
        X <- t(eX$vectors %*% diag(sqrt(pmax(eX$values, 
                                             0)), nvar) %*% t(X))
      }
      else {
        X <- r[sample(n.obs, n.obs, replace = TRUE), 
        ]
      }
      fs <- fac(X, nfactors = nfactors, rotate = rotate, 
                scores = "none", SMC = SMC, missing = missing, 
                impute = impute, min.err = min.err, max.iter = max.iter, 
                symmetric = symmetric, warnings = warnings, fm = fm, 
                alpha = alpha, oblique.scores = oblique.scores, 
                np.obs = np.obs, use = use, cor = cor, correct = correct, 
                n.rotations = n.rotations, hyper = hyper, ... = ...)
      if (nfactors == 1) {
        replicates <- list(loadings = fs$loadings)
      }
      else {
        t.rot <- target.rot(fs$loadings, fl)
        if (!is.null(fs$Phi)) {
          phis <- fs$Phi
          replicates <- list(loadings = t.rot$loadings, 
                             phis = phis[lower.tri(t.rot$Phi)])
        }
        else {
          replicates <- list(loadings = t.rot$loadings)
        }
      }
    })
    replicates <- matrix(unlist(replicateslist), nrow = n.iter, 
                         byrow = TRUE)
    means <- colMeans(replicates, na.rm = TRUE)
    sds <- apply(replicates, 2, sd, na.rm = TRUE)
    if (length(means) > (nvar * nfactors)) {
      means.rot <- means[(nvar * nfactors + 1):length(means)]
      sds.rot <- sds[(nvar * nfactors + 1):length(means)]
      ci.rot.lower <- means.rot + qnorm(p/2) * sds.rot
      ci.rot.upper <- means.rot + qnorm(1 - p/2) * sds.rot
      ci.rot <- data.frame(lower = ci.rot.lower, upper = ci.rot.upper)
    }
    else {
      rep.rots <- NULL
      means.rot <- NULL
      sds.rot <- NULL
      z.rot <- NULL
      ci.rot <- NULL
    }
    means <- matrix(means[1:(nvar * nfactors)], ncol = nfactors)
    sds <- matrix(sds[1:(nvar * nfactors)], ncol = nfactors)
    tci <- abs(means)/sds
    ptci <- 1 - pnorm(tci)
    if (!is.null(rep.rots)) {
      tcirot <- abs(means.rot)/sds.rot
      ptcirot <- 1 - pnorm(tcirot)
    }
    else {
      tcirot <- NULL
      ptcirot <- NULL
    }
    ci.lower <- means + qnorm(p/2) * sds
    ci.upper <- means + qnorm(1 - p/2) * sds
    ci <- data.frame(lower = ci.lower, upper = ci.upper)
    class(means) <- "loadings"
    colnames(means) <- colnames(sds) <- colnames(fl)
    rownames(means) <- rownames(sds) <- rownames(fl)
    f$cis <- list(means = means, sds = sds, ci = ci, p = 2 * 
                    ptci, means.rot = means.rot, sds.rot = sds.rot, ci.rot = ci.rot, 
                  p.rot = ptcirot, Call = cl, replicates = replicates, 
                  rep.rots = rep.rots)
    results <- f
    results$Call <- cl
    class(results) <- c("psych", "fa.ci")
  }
  else {
    results <- f
    results$Call <- cl
    class(results) <- c("psych", "fa")
  }
  return(results)
}

function (
    x, labels = NULL, cex = c(0.75, 1), main = "Biplot from fa", 
    hist.col = "cyan", xlim.s = c(-3, 3), ylim.s = c(-3, 3), 
    xlim.f = c(-1, 1), ylim.f = c(-1, 1), maxpoints = 100, adjust = 1.2, 
    col, pos, arrow.len = 0.1, pch = 16, choose = NULL, cuts = 1, 
    cutl = 0, group = NULL, smoother = FALSE, vars = TRUE, ...
) {
  if (is.null(x$scores)) stop()
  op <- par()
  old.par <- par(no.readonly = TRUE)
  on.exit(par(old.par))
  MAR <- par("mar")
  MFROW <- par("mfrow")
  title <- main
  main <- NULL
  if (is.list(x$scores)) 
    x$scores <- x$scores$scores
  if (is.list(x$fa)) 
    x$loadings <- x$fa$loadings
  if (!is.null(choose)) {
    x$scores <- x$scores[, choose, drop = FALSE]
    x$loadings <- x$loadings[, choose, drop = FALSE]
  }
  colnames(x$scores) <- colnames(x$loadings)
  if ((missing(group)) || (is.null(group))) 
    group <- rep(1, nrow(x$scores))
  n.dims <- dim(x$loadings)[2]
  if (missing(col)) {
    col <- c("black", "red", "blue", "#FF0000FF", "#00FF00FF", 
             "#00FFFFFF", "#0000FFFF", "#FF00FFFF")
  }
  ch.col <- col
  if (n.dims == 2) {
    op <- par(pty = "s")
    plotone(x$scores, x$loading, labels = labels, main = main, 
            xlim.s = xlim.s, ylim.s = ylim.s, xlim.f = xlim.f, 
            ylim.f = ylim.f, maxpoints = maxpoints, adjust = adjust, 
            col = col, pos = pos, arrow.len = arrow.len, pch = pch, 
            choose = choose, cuts = cuts, cutl = cutl, group = group, 
            ch.col = ch.col, smoother = smoother, vars = vars, 
            ...)
    par(new = TRUE)
    dev.hold()
    on.exit(dev.flush(), add = FALSE)
  }
  else {
    op1 <- par(mfrow = c(n.dims, n.dims), mar = c(2, 3, 3, 
                                                  2))
    if (nrow(x$scores) > maxpoints) {
      labels <- rep(".", nrow(x$scores))
    }
    else {
      labels <- rep("o", nrow(x$scores))
    }
    for (i in 1:n.dims) {
      for (j in 1:n.dims) {
        if (i == j) {
          h <- hist(x$scores[, i], freq = FALSE, main = colnames(x$loadings)[i], 
                    xlab = "", ylab = "", col = hist.col)
          breaks <- h$breaks
          nB <- length(breaks)
          tryd <- try(d <- density(x$scores[, i], na.rm = TRUE, 
                                   bw = "nrd", adjust = adjust), silent = TRUE)
          if (!inherits(tryd, "try-error")) {
            lines(d)
          }
        }
        else {
          plotone(x$scores[, c(j, i)], x$loadings[, c(j, 
                                                      i)], main = NULL, xlim.s = xlim.s, ylim.s = ylim.s, 
                  xlim.f = xlim.f, ylim.f = ylim.f, maxpoints = maxpoints, 
                  adjust = adjust, col = col, pos = pos, arrow.len = arrow.len, 
                  pch = pch, choose = choose, cuts = cuts, 
                  cutl = cutl, group = group, ch.col = ch.col, 
                  smoother = smoother, vars = vars, ...)
        }
      }
    }
  }
  title(title, line = 2)
}

function (scores, loadings, labels = NULL, cex = c(0.75, 1), 
          main = main, hist.col = "cyan", xlim.s = c(-3, 3), ylim.s = c(-3, 
                                                                        3), xlim.f = c(-1, 1), ylim.f = c(-1, 1), maxpoints = 100, 
          adjust = 1.2, col, pos, arrow.len = 0.1, pch = 16, choose = NULL, 
          cuts = 1, cutl = 0, group = NULL, ch.col = c("black", "blue"), 
          smoother = FALSE, vars = TRUE, ...) 
{
  choice <- "one"
  if (!is.null(labels)) {
    if (missing(pos)) {
      choice <- "two"
    }
    else {
      choice <- "three"
    }
  }
  if (smoother) 
    choice = "smoother"
  switch(choice, one = {
    plot(scores, xlim = xlim.s, ylim = ylim.s, cex = cex[1], 
         main = main, pch = pch[group], bg = ch.col[group], 
         col = col[group], ...)
  }, two = {
    plot(scores, typ = "n", xlim = xlim.s, ylim = ylim.s, 
         cex = cex[1], main = main, pch = pch[group], bg = ch.col[group], 
         col = col[group], ...)
    labels[sqrt((abs(scores[, 1])^2 + abs(scores[, 2])^2)) < 
             cuts] <- NA
    text(scores, labels = labels, col = ch.col[group], pos = NULL, 
         cex = cex[1])
  }, three = {
    plot(scores, xlim = xlim.s, ylim = ylim.s, cex = cex[1], 
         main = main, pch = pch[group], bg = ch.col[group], 
         col = col[group], ...)
    labels[sqrt((abs(scores[, 1])^2 + abs(scores[, 2])^2)) < 
             cuts] <- NA
    text(scores, labels = labels, pos = pos, cex = cex[1], 
         col = ch.col[group])
  }, smoother = {
    smoothScatter(scores, nrpoints = 0)
  })
  par(new = TRUE)
  dev.hold()
  on.exit(dev.flush(), add = FALSE)
  plot(loadings, axes = FALSE, type = "n", xlim = xlim.f, ylim = ylim.f, 
       xlab = "", ylab = "", col = col[1L], ...)
  labels <- rownames(loadings)
  labels[sqrt(loadings[, 1]^2 + loadings[, 2]^2) < cutl] <- NA
  text(loadings, labels = labels, cex = cex[2L], col = col[2L], 
       ...)
  if (vars) {
    arrows(0, 0, loadings[, 1L] * 0.8, loadings[, 2L] * 0.8, 
           col = col[2L], length = arrow.len)
  }
  else {
    arrows(0, 0, scores[, 1L] * xlim.f/xlim.s, scores[, 2L] * 
             ylim.f/ylim.s, col = col[2L], length = arrow.len)
  }
  axis(3, col = col[2L], ...)
  axis(4, col = col[2L], ...)
  box(col = col[1L])
}

# https://stackoverflow.com/q/25337134/4556798
library(psych)
data(bfi, package = "psych")
X <- tidyr::drop_na(bfi[seq(100), 16:25])

fa2 <- fa(X, 2L, scores = "regression")
head(fa2$scores)
biplot(
  fa2,
  pch=c(0,1)[bfi$gender],
  col=c("blue","red")[bfi$gender]
)

L2 <- unclass(fa2$loadings)
apply(L2, 2L, norm, "2")
sqrt(colSums(L2 ^ 2))
V2 <- fa2$residual
# estimated covariance matrix
hist(L2 %*% t(L2) + V2 - cor(X))
# calculate scores??
F2 <- X %*% solve(cor(X)) %*% L2
F2 <- t(t(L2) %*% solve(cor(X)) %*% t(X))

fa1 <- factanal(X, 2L, scores = "regression")
diag(proxy::dist(L2, fa1$loadings))
