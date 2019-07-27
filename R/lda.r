#' @title Augmented implementation of linear discriminant analysis
#'
#' @description This function replicates [MASS::lda()] with options to retain
#'   elements useful to the [tbl_ord] class and biplot calculations.
#'
#' @details
#'
#' Linear discriminant analysis relies on an eigendecomposition of the product
#' \eqn{W^{-1}B} of the inverse of the within-class covariance matrix \eqn{W} by
#' the between-class covariance matrix \eqn{B}. This eigendecomposition can be
#' motivated as the right (\eqn{V}) half of the singular value decomposition of
#' the matrix of _Mahalanobis distances_ between the cases after "sphering"
#' (linearly transforming them so that the within-class covariance is the
#' identity matrix). LDA are not traditionally represented as biplots, with some
#' exceptions (Gardner & le Roux, 2005; Greenacre, 2010, p. 109--117).
#'
#' LDA is implemented as [MASS::lda()] in the **MASS** package, in which the
#' variables are transformed by a sphering matrix \eqn{S} (Venables & Ripley,
#' 2003, p. 331--333). The returned element `scaling` contains the
#' unstandardized _discriminant coefficients_, which define the discriminant
#' scores of the cases and their centroids as linear combinations of the
#' original variables.
#'
#' The discriminant coefficients constitute one of several possible choices of
#' axes for a biplot representation of the LDA. The slightly modified function
#' [lda_ord()] provides additional options:
#'
#' * The _standardized discriminant coefficients_ are obtained by (re)scaling
#' the coefficients by the variable standard deviations. These coefficients
#' indicate the contributions of the variables to the discriminant scores after
#' controlling for their variances (ttnphns, 2013).

#' * The variables' _contributions_ to the Mahalanobis variance along each
#' discriminant axis are obtained by transforming the coefficients by the
#' inverse of the sphering matrix \eqn{S}. Because the contribution biplot
#' derives from the eigendecomposition of the Mahalanobis distance matrix, the
#' projections of the centroids and cases onto the variable axes approximate
#' their variable values after centering and sphering (Greenacre, 2013).
#' 

#' @template ref-gardner2005
#' @template ref-greenacre2010
#' @template ref-venables2003
#' @template ref-ttnphns2013
#' @template ref-greenacre2013

#' @name lda-ord
#' @include ord-tbl.r
#' @importFrom stats .getXlevels delete.response model.matrix model.response var
#' @inheritParams MASS::lda
#' @param axes.scale Character string indicating how to left-transform the
#'   `scaling` value when rendering biplots using [ggbiplot()]. Options include
#'   `"unstandardized"`, `"standardized"`, and `"contribution"`.
#' @param ret.x,ret.grouping Logical; whether to retain as attributes the data
#'   matrix (`x`) and the class assignments (`grouping`) on which LDA is
#'   performed. Methods like `predict()` access these objects by name in the
#'   parent environment, and retaining them as attributes prevents errors that
#'   arise if these objects are reassigned.
#' @example inst/examples/diabetes-lda-supplement.r
#' @example inst/examples/iris-lda.r
NULL

#' @rdname lda-ord
#' @export
lda_ord <- function(x, ...) UseMethod("lda_ord")

#' @rdname lda-ord
#' @export
lda_ord.formula <- function(formula, data, ..., subset, na.action)
{
  m <- match.call(expand.dots = FALSE)
  m$... <- NULL
  m[[1L]] <- quote(stats::model.frame)
  m <- eval.parent(m)
  Terms <- attr(m, "terms")
  grouping <- model.response(m)
  x <- model.matrix(Terms, m)
  xint <- match("(Intercept)", colnames(x), nomatch = 0L)
  if(xint > 0L) x <- x[, -xint, drop = FALSE]
  res <- lda_ord.default(x, grouping, ...)
  res$terms <- Terms
  ## fix up call to refer to the generic, but leave arg name as `formula'
  cl <- match.call()
  cl[[1L]] <- as.name("lda_ord")
  res$call <- cl
  res$contrasts <- attr(x, "contrasts")
  res$xlevels <- .getXlevels(Terms, m)
  res$na.action <- attr(m, "na.action")
  res
}

#' @rdname lda-ord
#' @export
lda_ord.data.frame <- function(x, ...)
{
  res <- lda_ord(structure(data.matrix(x), class = "matrix"), ...)
  cl <- match.call()
  cl[[1L]] <- as.name("lda_ord")
  res$call <- cl
  res
}

#' @rdname lda-ord
#' @export
lda_ord.matrix <- function(x, grouping, ..., subset, na.action)
{
  if(!missing(subset)) {
    x <- x[subset, , drop = FALSE]
    grouping <- grouping[subset]
  }
  if(!missing(na.action)) {
    dfr <- na.action(structure(list(g = grouping, x = x),
                               class = "data.frame"))
    grouping <- dfr$g
    x <- dfr$x
  }
  #    res <- NextMethod("lda_ord")
  res <- lda_ord.default(x, grouping, ...)
  cl <- match.call()
  cl[[1L]] <- as.name("lda_ord")
  res$call <- cl
  res
}

#' @rdname lda-ord
#' @export
lda_ord.default <- function(x, grouping, prior = proportions, tol = 1.0e-4,
                            method = c("moment", "mle", "mve", "t"),
                            CV = FALSE, nu = 5, ...,
                            ret.x = FALSE, ret.grouping = FALSE,
                            axes.scale = "unstandardized")
{
  if(is.null(dim(x))) stop("'x' is not a matrix")
  x <- as.matrix(x)
  if(any(!is.finite(x)))
    stop("infinite, NA or NaN values in 'x'")
  n <- nrow(x)
  p <- ncol(x)
  if(n != length(grouping))
    stop("nrow(x) and length(grouping) are different")
  g <- as.factor(grouping)
  lev <- lev1 <- levels(g)
  counts <- as.vector(table(g))
  if(!missing(prior)) {
    if(any(prior < 0) || round(sum(prior), 5) != 1) stop("invalid 'prior'")
    if(length(prior) != nlevels(g)) stop("'prior' is of incorrect length")
    prior <- prior[counts > 0L]
  }
  if(any(counts == 0L)) {
    empty <- lev[counts == 0L]
    warning(sprintf(ngettext(length(empty),
                             "group %s is empty",
                             "groups %s are empty"),
                    paste(empty, collapse = " ")), domain = NA)
    lev1 <- lev[counts > 0L]
    g <- factor(g, levels = lev1)
    counts <- as.vector(table(g))
  }
  proportions <- counts/n
  ng <- length(proportions)
  names(prior) <- names(counts) <- lev1
  method <- match.arg(method)
  if(CV && !(method == "moment" || method == "mle"))
    stop(gettext("cannot use leave-one-out CV with method %s",
                 sQuote(method)), domain = NA)
  ## drop attributes to avoid e.g. matrix() methods
  group.means <- tapply(c(x), list(rep(g, p), col(x)), mean)
  covw <- var(x - group.means[g,  ])
  if (axes.scale == "contribution") covw.eig <- eigen(covw)
  f1 <- sqrt(diag(covw))
  if(any(f1 < tol)) {
    const <- format((1L:p)[f1 < tol])
    stop(sprintf(ngettext(length(const),
                          "variable %s appears to be constant within groups",
                          "variables %s appear to be constant within groups"),
                 paste(const, collapse = " ")),
         domain = NA)
  }
  # scale columns to unit variance before checking for collinearity
  scaling <- diag(1/f1, , p)
  if(method == "mve") {
    # adjust to "unbiased" scaling of covariance matrix
    cov <- n/(n - ng) * MASS::cov.rob((x - group.means[g,  ]) %*% scaling)$cov
    sX <- svd(cov, nu = 0L)
    rank <- sum(sX$d > tol^2)
    if(rank == 0L) stop("rank = 0: variables are numerically constant")
    if(rank < p) warning("variables are collinear")
    scaling <- scaling %*% sX$v[, 1L:rank] %*%
      diag(sqrt(1/sX$d[1L:rank]),,rank)
  } else if(method == "t") {
    if(nu <= 2) stop("'nu' must exceed 2")
    w <- rep(1, n)
    repeat {
      w0 <- w
      X <- x - group.means[g, ]
      sX <- svd(sqrt((1 + p/nu)*w/n) * X, nu = 0L)
      X <- X %*% sX$v %*% diag(1/sX$d,, p)
      w <- 1/(1 + drop(X^2 %*% rep(1, p))/nu)
      print(summary(w))
      group.means <- tapply(w*x, list(rep(g, p), col(x)), sum)/
        rep.int(tapply(w, g, sum), p)
      if(all(abs(w - w0) < 1e-2)) break
    }
    X <-  sqrt(nu/(nu-2)*(1 + p/nu)/n * w) * (x - group.means[g,  ]) %*% scaling
    X.s <- svd(X, nu = 0L)
    rank <- sum(X.s$d > tol)
    if(rank == 0L) stop("rank = 0: variables are numerically constant")
    if(rank < p) warning("variables are collinear")
    scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],,rank)
  } else {
    fac <- if(method == "moment") 1/(n-ng) else 1/n
    X <- sqrt(fac) * (x - group.means[g,  ]) %*% scaling
    X.s <- svd(X, nu = 0L)
    rank <- sum(X.s$d > tol)
    if(rank == 0L) stop("rank = 0: variables are numerically constant")
    if(rank < p) warning("variables are collinear")
    scaling <- scaling %*% X.s$v[, 1L:rank] %*% diag(1/X.s$d[1L:rank],,rank)
  }
  # now have variables scaled so that W is the identity
  if(CV) {
    x <- x %*% scaling
    dm <- group.means %*% scaling
    K <- if(method == "moment") ng else 0L
    dist <- matrix(0, n, ng)
    for(i in 1L:ng) {
      dev <- x - matrix(dm[i,  ], n, rank, byrow = TRUE)
      dist[, i] <- rowSums(dev^2)
    }
    ind <- cbind(1L:n, g)
    nc <- counts[g]
    cc <- nc/((nc-1)*(n-K))
    dist2 <- dist
    for(i in 1L:ng) {
      dev <- x - matrix(dm[i,  ], n, rank, byrow = TRUE)
      dev2 <- x - dm[g, ]
      tmp <- rowSums(dev*dev2)
      dist[, i] <- (n-1L-K)/(n-K) * (dist2[, i] +  cc*tmp^2/(1 - cc*dist2[ind]))
    }
    dist[ind] <- dist2[ind] * (n-1L-K)/(n-K) * (nc/(nc-1))^2 /
      (1 - cc*dist2[ind])
    dist <- 0.5 * dist - matrix(log(prior), n, ng, byrow = TRUE)
    dist <- exp(-(dist - min(dist, na.rm = TRUE)))
    cl <- factor(lev1[max.col(dist)], levels = lev)
    ##  convert to posterior probabilities
    posterior <- dist/drop(dist %*% rep(1, length(prior)))
    dimnames(posterior) <- list(rownames(x), lev1)
    return(list(class = cl, posterior = posterior))
  }
  xbar <- colSums(prior %*% group.means)
  fac <- if(method == "mle") 1/ng else 1/(ng - 1)
  X <- sqrt((n * prior)*fac) *
    scale(group.means, center = xbar, scale = FALSE) %*% scaling
  X.s <- svd(X, nu = 0L)
  rank <- sum(X.s$d > tol * X.s$d[1L])
  if(rank == 0L) stop("group means are numerically identical")
  scaling <- scaling %*% X.s$v[, 1L:rank]
  if(is.null(dimnames(x)))
    dimnames(scaling) <- list(NULL, paste("LD", 1L:rank, sep = ""))
  else {
    dimnames(scaling) <- list(colnames(x), paste("LD", 1L:rank, sep = ""))
    dimnames(group.means)[[2L]] <- colnames(x)
  }
  cl <- match.call()
  cl[[1L]] <- as.name("lda_ord")
  res <- structure(list(
    prior = prior, counts = counts, means = group.means,
    scaling = scaling, lev = lev, svd = X.s$d[1L:rank],
    N = n, call = cl
  ), class = c("lda_ord", "lda"))
  if (ret.x) attr(res, "x") <- x
  if (ret.grouping) attr(res, "grouping") <- grouping
  attr(res, "axes.scale") <- switch(
    axes.scale,
    # unstandardized discriminant coefficients
    unstandardized = NULL,
    # standardized discriminant coefficients
    standardized = diag(f1),
    # un-transformed discriminant coefficients (approximates inner products)
    contribution = covw.eig$vectors %*%
      diag(sqrt(covw.eig$values)) %*%
      t(covw.eig$vectors),
    # pooled within-group correlations
    within.corr = {
      warning(
        "Cannot recover pooled within-group correlations from returned values;",
        " variable axes will not be scaled."
      )
      NULL
    }
  )
  res
}
