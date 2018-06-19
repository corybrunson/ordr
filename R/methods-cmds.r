#' Bibble functionality for classical multidimensional scaling objects
#' 
#' These methods extract data from, and attribute new data to, objects of class 
#' \code{"cmds"}. (This is a class introduced in this package to identify 
#' objects returned by \code{\link[stats]{cmdscale}}, which is masked by a
#' wrapper that adds the class attribute.)
#' 
#' @name methods-cmds
#' @template methods-params
#' @template matrix-param
#' @example inst/examples/ex-bibble-cmds.r

#' @rdname methods-cmds
#' @export
as_bibble.cmds <- as_bibble_recognized

recover_uv_cmds <- function(x, .matrix) {
  .matrix <- match_factor(.matrix)
  res <- x$points
  dimnames(res) <- list(
    dimnames(x$x)[[switch(.matrix, u = 1, v = 2)]],
    recover_coord(x)
  )
  res
}

#' @rdname methods-cmds
#' @export
recover_u.cmds <- function(x) recover_uv_cmds(x, "u")

#' @rdname methods-cmds
#' @export
recover_v.cmds <- function(x) recover_uv_cmds(x, "v")

#' @rdname methods-cmds
#' @export
recover_coord.cmds <- function(x) paste0("PCo", 1:ncol(x$points))

#' @rdname methods-cmds
#' @export
augment_u.cmds <- function(x) {
  .name <- rownames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cmds
#' @export
augment_v.cmds <- function(x) {
  .name <- colnames(x$x)
  res <- if (is.null(.name)) {
    tibble_pole(ncol(x$x))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-cmds
#' @export
augment_coord.cmds <- function(x) {
  tibble(
    .name = recover_coord(x),
    .eig = x$eig[1:ncol(x$points)]
  )
}

#' @rdname methods-cmds
#' @export
negate_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get negations
  s <- negation_to(get_factor(as_bibble(x), .matrix), y)
  # tag 'cmds' object with negation
  x <- attribute_alignment(x, diag(s, nrow = ncol(x$points)))
  # return annotated object
  x
}

#' @rdname methods-cmds
#' @export
permute_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get permutation
  p <- permutation_to(get_factor(as_bibble(x), .matrix), y)
  # tag 'cmds' object with permutation
  x <- attribute_alignment(x, diag(1, nrow = ncol(x$points))[, p, drop = FALSE])
  # return annotated object
  x
}

#' @rdname methods-cmds
#' @export
rotate_to.cmds <- function(x, y, ..., .matrix) {
  y <- as.matrix(y, .matrix = .matrix)
  # get rotation matrix
  r <- rotation_to(get_factor(as_bibble(x), .matrix), y)
  # tag 'cmds' object with rotation
  x <- attribute_alignment(x, r)
  # return annotated object
  x
}
