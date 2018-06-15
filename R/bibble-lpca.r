
#' @importFrom stats plogis

#' @rdname bibble
#' @example inst/examples/ex-bibble-lpca.r
#' @export
as_bibble.lpca <- as_bibble_recognized

get_uv_lpca <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "PCs", v = "U")]]
  colnames(res) <- get_coord(x)
  res
}

#' @rdname bibble-factors
#' @export
get_u.lpca <- function(x) get_uv_lpca(x, "u")

#' @rdname bibble-factors
#' @export
get_v.lpca <- function(x) get_uv_lpca(x, "v")

#' @rdname bibble-factors
#' @export
get_coord.lpca <- function(x) paste0("LPC", 1:ncol(x$U))

#' @rdname bibble-annotation
#' @export
u_annot.lpca <- function(x) {
  .name <- rownames(x$PCs)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$PCs))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname bibble-annotation
#' @export
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

#' @rdname bibble-annotation
#' @export
coord_annot.lpca <- function(x) {
  tibble(
    .name = get_coord.lpca(x)
  )
}

#' @rdname align-to
#' @export
negate_to.lpca <- function(x, y, ..., .matrix) {
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

#' @rdname bibble
#' @export
as_bibble.lsvd <- as_bibble_recognized

get_uv_lsvd <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x[[switch(.matrix, u = "A", v = "B")]]
  colnames(res) <- get_coord(x)
  res
}

#' @rdname bibble-factors
#' @export
get_u.lsvd <- function(x) get_uv_lsvd(x, "u")

#' @rdname bibble-factors
#' @export
get_v.lsvd <- function(x) get_uv_lsvd(x, "v")

#' @rdname bibble-factors
#' @export
get_coord.lsvd <- function(x) paste0("LSC", 1:ncol(x$A))

#' @rdname bibble-annotation
#' @export
u_annot.lsvd <- function(x) {
  tibble(
    .name = rownames(x$A)
  )
}

#' @rdname bibble-annotation
#' @export
v_annot.lsvd <- function(x) {
  tibble(
    .name = rownames(x$B),
    .mu = x$mu
  )
}

#' @rdname bibble-annotation
#' @export
coord_annot.lsvd <- function(x) {
  tibble(
    .name = get_coord.lsvd(x)
  )
}

#' @rdname reconstruct
#' @export
reconstruct.lsvd <- function(x) {
  round(plogis(x$A %*% t(x$B)), 0)
}

#' @rdname align-to
#' @export
negate_to.lpca <- function(x, y, ..., .matrix) {
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
