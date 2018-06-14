
logisticPCA <- function(
  x, k = 2, m = 4,
  quiet = TRUE, partial_decomp = FALSE,
  max_iters = 1000, conv_criteria = 1e-05,
  random_start = FALSE,
  ...,
  main_effects = TRUE
) {
  lpca <- logisticPCA::logisticPCA(
    x, k = k,
    m = m, quiet = quiet, partial_decomp = partial_decomp,
    max_iters = max_iters, conv_criteria = conv_criteria,
    random_start = random_start,
    ...,
    main_effects = main_effects
  )
  rownames(lpca$U) <- colnames(x)
  #rownames(lpca$PCs) <- rownames(x)
  lpca
}

as_bibble.lpca <- as_bibble_recognized

get_uv_lpca <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "PCs", v = "U")]]
  colnames(res) <- get_coord(x)
  res
}
get_u.lpca <- function(x) get_uv_lpca(x, "u")
get_v.lpca <- function(x) get_uv_lpca(x, "v")

get_coord.lpca <- function(x) paste0("LPC", 1:ncol(x$U))

u_annot.lpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

v_annot.lpca <- function(x) {
  .name <- rownames(x$U)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$U))
  } else {
    tibble(.name = .name)
  }
  res$.mu <- x$mu
  res
}

coord_annot.lpca <- function(x) {
  tibble(
    .name = get_coord.lpca(x)
  )
}

negate_to.lpca <- function(x, y, .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_bibble(x), .matrix), y)
  # edit the 'cmds' object (doesn't depend on `.matrix`)
  x$PCs <- sweep(x$PCs, 2, s, "*")
  x$U <- sweep(x$U, 2, s, "*")
  # tag 'cmds' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$U)))
  # return rotated 'cmds' object
  x
}

logisticSVD <- function(
  x, k = 2,
  quiet = TRUE, max_iters = 1000,
  conv_criteria = 1e-05, random_start = FALSE,
  ...,
  partial_decomp = TRUE, main_effects = TRUE
) {
  lsvd <- logisticPCA::logisticSVD(
    x = x, k = k,
    quiet = quiet, max_iters = max_iters,
    conv_criteria = conv_criteria, random_start = random_start,
    ...,
    partial_decomp = partial_decomp, main_effects = main_effects
  )
  rownames(lsvd$A) <- rownames(x)
  rownames(lsvd$B) <- colnames(x)
  lsvd
}

as_bibble.lsvd <- as_bibble_recognized

get_uv_lsvd <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "A", v = "B")]]
  colnames(res) <- get_coord(x)
  res
}
get_u.lsvd <- function(x) get_uv_lsvd(x, "u")
get_v.lsvd <- function(x) get_uv_lsvd(x, "v")

get_coord.lsvd <- function(x) paste0("LSC", 1:ncol(x$A))

u_annot.lsvd <- function(x) {
  tibble(
    .name = rownames(x$A)
  )
}

v_annot.lsvd <- function(x) {
  tibble(
    .name = rownames(x$B),
    .mu = x$mu
  )
}

coord_annot.lsvd <- function(x) {
  tibble(
    .name = get_coord.lsvd(x)
  )
}

reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}

negate_to.lpca <- function(x, y, .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_bibble(x), .matrix), y)
  # edit the 'cmds' object (doesn't depend on `.matrix`)
  x$A <- sweep(x$A, 2, s, "*")
  x$B <- sweep(x$B, 2, s, "*")
  # tag 'cmds' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$B)))
  # return rotated 'cmds' object
  x
}
