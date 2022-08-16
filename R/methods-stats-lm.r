#' @title Functionality for linear model objects
#'
#' @description These methods extract data from, and attribute new data to,
#'   objects of class `"lm"`, `"glm"`, and `"mlm"` as returned by [stats::lm()]
#'   and [stats::glm()].
#'
#' @name methods-lm
#' @include ord-tbl.r
#' @importFrom stats model.frame influence cooks.distance predict
#' @template param-methods
#' @template return-methods
#' @family methods for idiosyncratic techniques
#' @example inst/examples/ex-methods-lm-mtcars.r
NULL

#' @rdname methods-lm
#' @export
as_tbl_ord.lm <- as_tbl_ord_default

#' @rdname methods-lm
#' @export
recover_rows.lm <- function(x) {
  res <- model.matrix(x)
  if (is.null(rownames(res)))
    rownames(res) <- rownames(x$model[, -1, drop = FALSE])
  if (is.null(colnames(res))) colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_cols.lm <- function(x) {
  res <- t(x$coefficients)
  if (is.null(rownames(res))) rownames(res) <-
      if (is.matrix(x$model[, 1])) colnames(x$model[, 1]) else names(x$model)[1]
  if (is.null(colnames(res))) colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_coord.lm <- function(x) {
  if (is.matrix(x$coefficients))
    colnames(x$coefficients) else names(x$coefficients)
}

#' @rdname methods-lm
#' @export
recover_aug_rows.lm <- function(x) {
  res <- tibble(name = rownames(model.frame(x)))
  infl <- influence(x, do.coef = FALSE)
  # diagnostics
  res$hat <- infl$hat
  res$sigma <- infl$sigma
  res$cooksd <- cooks.distance(x, infl = infl)
  # residuals
  res$wt.res <- infl$wt.res
  # predictions
  pred <- as.data.frame(predict(x, se.fit = TRUE)[1:2])
  names(pred) <- paste0(".", names(pred))
  res <- bind_cols(res, pred)
  # augmentation!
  res
}

#' @rdname methods-lm
#' @export
recover_aug_cols.lm <- function(x) {
  name <- if (is.matrix(x$model[, 1]) && ! is.null(colnames(x$model[, 1]))) {
    colnames(x$model[, 1])
  } else {
    names(x$model)[1]
  }
  tibble(name = name)
}

#' @rdname methods-lm
#' @export
recover_aug_coord.lm <- function(x) {
  summ <- as.data.frame(stats::coef(summary(x)))
  names(summ) <- c("estimate", "std.error", "t.value", "p.value")
  as_tibble(data.frame(
    name = factor_coord(recover_coord(x)),
    summ
  ))
}

#' @rdname methods-lm
#' @export
recover_aug_rows.glm <- function(x) {
  res <- tibble(name = rownames(model.frame(x)))
  # diagnostics
  infl <- influence(x, do.coef = FALSE)
  zero_wt <- as.numeric(x$weights != 0)
  res$hat <- infl$hat * zero_wt
  res$sigma <- infl$sigma * zero_wt
  res$cooksd <- cooks.distance(x, infl = infl)
  # residuals
  res$dev.res <- infl$dev.res
  res$pear.res <- infl$pear.res
  # all predictions (because why not)
  for (tp in c("link", "response", "terms")) {
    pred <- as.data.frame(predict(x, type = tp, se.fit = TRUE)[1:2])
    names(pred) <- paste0(".", tp, ".", names(pred))
    res <- bind_cols(res, pred)
  }
  # augmentation!
  res
}

#' @rdname methods-lm
#' @export
recover_rows.mlm <- function(x) {
  .intercept_col <- if (rownames(x$coefficients)[1] == "(Intercept)") {
    .ic <- matrix(1L, nrow = nrow(x$model), ncol = 1)
    colnames(.ic) <- "(Intercept)"
    .ic
  } else matrix(NA_integer_, nrow = nrow(x$model), ncol = 0)
  .predictors <- as.matrix(model.frame(x)[, -1])
  res <- cbind(.intercept_col, .predictors)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_cols.mlm <- function(x) {
  res <- t(x$coefficients)
  colnames(res) <- recover_coord(x)
  res
}

#' @rdname methods-lm
#' @export
recover_coord.mlm <- function(x) {
  .predictors <- x$model[, -1]
  if (is.matrix(.predictors)) {
    coord <- colnames(.predictors)
  } else {
    coord <- names(.predictors)
    mat_coord <- which(sapply(
      .predictors,
      function(y) is.matrix(y) && ! is.null(colnames(y))
    ))
    coord[mat_coord] <- unname(unlist(lapply(
      mat_coord,
      function(i) colnames(.predictors[, i])
    )))
  }
  if (rownames(x$coefficients)[1] == "(Intercept)") {
    coord <- c("(Intercept)", coord)
  }
  coord
}

#' @rdname methods-lm
#' @export
recover_aug_rows.mlm <- function(x) {
  tibble(
    name = rownames(model.frame(x))
  )
}

#' @rdname methods-lm
#' @export
recover_aug_cols.mlm <- function(x) {
  name <- colnames(x$coefficients)
  if (is.null(name)) {
    name <- paste(names(x$model)[1], 1:ncol(x$model[, 1]), sep = ".")
  }
  tibble(name = name)
}

#' @rdname methods-lm
#' @export
recover_aug_coord.mlm <- function(x) {
  # model summaries
  summs <- purrr::map_df(
    stats::coef(summary(x)),
    as_tibble,
    rownames = "term",
    .id = "response"
  )
  names(summs)[seq(3L, 6L)] <- c("estimate", "std.error", "t.value", "p.value")
  summs$response <- gsub("^Response ", "", summs$response)
  res <- as_tibble(data.frame(
    name = factor_coord(recover_coord(x)),
    summs
  ))
  # nest to coordinates
  tidyr::nest(res, summary = -dplyr::one_of("name", "term"))
}
