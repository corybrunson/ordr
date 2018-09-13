#' Functionality for partial least squares (latent factors regression?) objects
#' 
#' These methods access and annotate objects of class \code{"mvr"}.
#' 
#' @name methods-mvr
#' @template methods-params
#' @template matrix-param
#' @example inst/examples/ex-mvr.r

#' @rdname methods-mvr
#' @export
as_tbl_ord.mvr <- function(x) {
  class(x) <- c("tbl_ord", class(x))
  design(x) <- "responses"
  x
}

#' @rdname methods-mvr
#' @export
designate.mvr <- function(.data, what) {
  value <- rlang::quo_text(enquo(what))
  value <- gsub("\"", "", value)
  designees <- c("predictors", "responses")
  value <- pmatch(value, designees)
  if (is.na(value)) designee_error("mvr", designees)
  design(.data) <- value
  .data
}

#' @rdname methods-mvr
#' @export
recover_u.mvr <- function(x) {
  res <- switch(
    design(x),
    predictors = unclass(x$scores),
    responses = unclass(x$Yscores)
  )
  res
}

#' @rdname methods-mvr
#' @export
recover_v.mvr <- function(x) {
  res <- switch(
    design(x),
    predictors = unclass(x$loadings),
    responses = unclass(x$Yloadings)
  )
  res
}

#' @rdname methods-mvr
#' @export
recover_coord.mvr <- function(x) {
  colnames(x$scores)
}

#' @rdname methods-mvr
#' @export
augment_u.mvr <- function(x) {
  u <- recover_u(x)
  .name <- rownames(u)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(u))
  } else {
    tibble(.name = .name)
  }
  res
}

#' @rdname methods-mvr
#' @export
augment_v.mvr <- function(x) {
  v <- recover_v(x)
  .name <- rownames(v)
  res <- if (is.null(.name)) {
    tibble_pole(nrow(v))
  } else {
    tibble(.name = .name)
  }
  res$.means <- switch(
    design(x),
    predictors = x$Xmeans,
    responses = x$Ymeans
  )
  res
}

#' @rdname methods-mvr
#' @export
augment_coord.mvr <- function(x) {
  tibble(
    .name = recover_coord(x),
    .Xvar = x$Xvar
  )
}

#' @rdname designation
#' @export
designate <- function(.data, what) UseMethod("designate")

#' @rdname designation
#' @export
design <- function(x) {
  attr(x, "design")
}

#' @rdname designation
#' @export
`design<-` <- function(x, value) {
  attr(x, "design") <- value
  x
}

designee_error <- function(class, designees) {
  stop(
    "Designate one of the following for a '", class, "' object: ",
    paste(designees, collapse = ", ")
  )
}
