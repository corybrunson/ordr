
#' @rdname bibble
#' @export
as_bibble.cmds <- as_bibble_recognized

get_uv_cmds <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x$points
  dimnames(res) <- list(
    dimnames(x$x)[[switch(.matrix, u = 1, v = 2)]],
    get_coord(x)
  )
  res
}

#' @rdname bibble-factors
#' @export
get_u.cmds <- function(x) get_uv_cmds(x, "u")

#' @rdname bibble-factors
#' @export
get_v.cmds <- function(x) get_uv_cmds(x, "v")

#' @rdname bibble-factors
#' @export
get_coord.cmds <- function(x) paste0("PCo", 1:ncol(x$points))

#' @rdname bibble-annotation
#' @export
u_annot.cmds <- function(x) {
  .name <- rownames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname bibble-annotation
#' @export
v_annot.cmds <- function(x) {
  .name <- colnames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname bibble-annotation
#' @export
coord_annot.cmds <- function(x) {
  tibble(
    .name = get_coord(x),
    .eig = x$eig[1:ncol(x$points)]
  )
}

#' @rdname align-to
#' @export
negate_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_bibble(x), .matrix), y)
  # edit the 'cmds' object (doesn't depend on `.matrix`)
  x$points <- sweep(x$points, 2, s, "*")
  # tag 'cmds' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$points)))
  # return rotated 'cmds' object
  x
}

#' @rdname align-to
#' @export
permute_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get permutation
  p <- permutation_to(get_factor(as_bibble(x), .matrix), y)
  # edit the 'cmds' object (doesn't depend on `.matrix`)
  x$points <- x$points[, p, drop = FALSE]
  # tag 'cmds' object with permutation
  x <- attribute_alignment(x, diag(1, nrow = ncol(x$points))[, p, drop = FALSE])
  # return rotated 'cmds' object
  x
}

#' @rdname align-to
#' @export
rotate_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get rotation matrix
  r <- rotation_to(get_factor(as_bibble(x), .matrix), y)
  # edit the 'cmds' object (doesn't depend on `.matrix`)
  x$points <- x$points %*% r
  # tag 'cmds' object with rotation
  x <- attribute_alignment(x, r)
  # return rotated 'cmds' object
  x
}
