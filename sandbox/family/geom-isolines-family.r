# from previous "geom-isolines.r":


# calibrate axis range according to intercept and family
if (calibrate) {
  ran_vars <- c("unitmin", "unitmax")
  data[, ran_vars] <- data[, ran_vars] + data$intercept
  if (! is.null(family_fun)) {
    data[, ran_vars] <- family_fun$linkinv(as.matrix(data[, ran_vars]))
  }
}

# element units; by default, use Wilkinson's breaks algorithm
if (is.null(by)) {
  bys <- lapply(1:nrow(data), function(i) {
    labeling::extended(data$unitmin[i], data$unitmax[i], 6)
  })
} else {
  if (length(by) == 1) by <- rep(by, nrow(data))
  bys <- lapply(1:nrow(data), function(i) {
    floor(data$unitmin[i] / by[i]):ceiling(data$unitmax[i] / by[i]) * by[i]
  })
}
data <- data[rep(1:nrow(data), sapply(bys, length)), , drop = FALSE]
data$units <- unlist(bys)
data$unitmin <- NULL
data$unitmax <- NULL

# un-calibrate axis units according to intercept and family
if (calibrate) {
  unit_vars <- c("units")
  if (! is.null(family_fun)) {
    data[, unit_vars] <- family_fun$linkfun(as.matrix(data[, unit_vars]))
  }
  data[, unit_vars] <- data[, unit_vars] - data$intercept
}
