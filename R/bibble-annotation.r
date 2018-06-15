#' Recover annotation for subjects, variables, or coordinates
#' 

#' @name bibble-annotation
#' @inheritParams bibble-factors

#' @rdname bibble-annotation
#' @export
u_annot <- function(x) UseMethod("u_annot")

#' @rdname bibble-annotation
#' @export
v_annot <- function(x) UseMethod("v_annot")

#' @rdname bibble-annotation
#' @export
factor_annot <- function(x, .matrix) {
  switch(
    match_factor(.matrix),
    u = u_annot(x),
    v = v_annot(x),
    uv = list(u = u_annot(x), v = v_annot(x))
  )
}

#' @rdname bibble-annotation
#' @export
coord_annot <- function(x) UseMethod("coord_annot")
