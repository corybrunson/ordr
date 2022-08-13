#' @title **dplyr** verbs for tbl_ord factors
#'
#' @description These functions adapt [dplyr][dplyr::dplyr] verbs to the factors
#'   of a [tbl_ord].
#'
#' @description The raw verbs are not defined for tbl_ords; instead, each verb
#'   has two analogues, corresponding to the two matrix factors. They each rely
#'   on a common workhorse function, which takes the composition of the
#'   **dplyr** verb with `annotation_*`, applied to the factor, removes any
#'   variables corresponding to coordinates or already annotated, and only then
#'   assigns it as the new `"*_annotation"` attribute of `.data` (see
#'   [annotation]). Note that these functions are not generics and so cannot be
#'   extended to other classes.
#'   

#' @name dplyr-verbs
#' @importFrom tidyselect one_of
#' @importFrom dplyr pull rename select mutate transmute bind_rows bind_cols
#'   left_join
#' @param .data An object of class '[tbl_ord]'.
#' @param var A variable specified as in [dplyr::pull()].
#' @param ... Comma-separated unquoted expressions as in, e.g.,
#'   [dplyr::select()].
#' @template param-elements
#' @template param-matrix
#' @return A tbl_ord; the wrapped model is unchanged.
#' @example inst/examples/ex-dplyr-verbs-iris-lda.r

pull_factor <- function(.data, var = -1, .matrix) {
  crd <- as_tibble(get_factor(.data, .matrix = .matrix))
  att <- annotation_factor(.data, .matrix = .matrix)
  tbl <- bind_cols(crd, att)
  pull(tbl, !! enquo(var))
}
#' @rdname dplyr-verbs
#' @export
pull_rows <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
pull_cols <- function(.data, var = -1) {
  pull_factor(.data, !! enquo(var), .matrix = "cols")
}

rename_factor <- function(.data, ..., .matrix) {
  att <- rename(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
rename_rows <- function(.data, ...) {
  rename_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
rename_cols <- function(.data, ...) {
  rename_factor(.data, ..., .matrix = "cols")
}

select_factor <- function(.data, ..., .matrix) {
  att <- select(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
select_rows <- function(.data, ...) {
  select_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
select_cols <- function(.data, ...) {
  select_factor(.data, ..., .matrix = "cols")
}

mutate_factor <- function(.data, ..., .matrix) {
  crd <- as_tibble(get_factor(.data, .matrix = .matrix))
  att <- annotation_factor(.data, .matrix = .matrix)
  tbl <- bind_cols(crd, att)
  tbl <- mutate(tbl, ...)
  tbl <- select(tbl, -one_of(intersect(names(crd), names(tbl))))
  set_annotation_factor(.data, tbl, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
mutate_rows <- function(.data, ...) {
  mutate_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
mutate_cols <- function(.data, ...) {
  mutate_factor(.data, ..., .matrix = "cols")
}

transmute_factor <- function(.data, ..., .matrix) {
  crd <- as_tibble(get_factor(.data, .matrix = .matrix))
  att <- annotation_factor(.data, .matrix = .matrix)
  tbl <- bind_cols(crd, att)
  tbl <- transmute(tbl, ...)
  tbl <- select(tbl, -one_of(intersect(names(crd), names(tbl))))
  set_annotation_factor(.data, tbl, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
transmute_rows <- function(.data, ...) {
  transmute_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
transmute_cols <- function(.data, ...) {
  transmute_factor(.data, ..., .matrix = "cols")
}

cbind_factor <- function(.data, ..., .matrix, elements = "all") {
  ann_fac <- annotation_factor(.data, .matrix = .matrix)
  att_fac <- if (elements == "all" || ! ".element" %in% names(ann_fac)) {
    tibble(...)
  } else {
    elts_rows <- ann_fac$.element == elements
    if (! any(elts_rows)) {
      warning("No ", elements, " elements found.")
      tibble_pole(nrow = nrow(ann_fac))
    } else {
      n_above <- min(which(elts_rows)) - 1L
      n_below <- min(which(rev(elts_rows))) - 1L
      tbl_above <- if (n_above > 1L) tibble_pole(nrow = n_above)
      tbl_below <- if (n_below > 1L) tibble_pole(nrow = n_below)
      bind_rows(tbl_above, ..., tbl_below)
    }
  }
  att <- if (nrow(ann_fac) == 0L) att_fac else bind_cols(ann_fac, att_fac)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
cbind_rows <- function(.data, ..., elements = "all") {
  cbind_factor(.data, ..., .matrix = "rows", elements = elements)
}
#' @rdname dplyr-verbs
#' @export
cbind_cols <- function(.data, ..., elements = "all") {
  cbind_factor(.data, ..., .matrix = "cols", elements = elements)
}

left_join_factor <- function(.data, ..., .matrix) {
  att <- left_join(annotation_factor(.data, .matrix = .matrix), ...)
  set_annotation_factor(.data, att, .matrix = .matrix)
}
#' @rdname dplyr-verbs
#' @export
left_join_rows <- function(.data, ...) {
  left_join_factor(.data, ..., .matrix = "rows")
}
#' @rdname dplyr-verbs
#' @export
left_join_cols <- function(.data, ...) {
  left_join_factor(.data, ..., .matrix = "cols")
}
