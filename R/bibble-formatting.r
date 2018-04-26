
#' @rdname formatting
#' @export
print.bbl <- function(x, n = 4, ...) {
  stopifnot(is.bibble(x))
  arg_list <- list(...)
  
  # components
  prev_class <- setdiff(c(attr(x, "preclass"), class(x)), "bbl")[1]
  wrapped <- is.null(attr(x, "preclass"))
  biplot_descr <- if (wrapped) {
    paste0("# A bibble-wrapped object of class '", prev_class, "':")
  } else if (!is.null(prev_class) && prev_class != "list") {
    paste0("# A bibble obtained from a class-'", prev_class, "' object:")
  } else {
    paste0("# A bibble:")
  }
  u <- get_u(x)
  u_trunc <- tibble::trunc_mat(u, n = n, ...)
  u_trunc$summary <- gsub("data\\.frame", "U", u_trunc$summary)
  v <- get_v(x)
  v_trunc <- tibble::trunc_mat(v, n = n, ...)
  v_trunc$summary <- gsub("data\\.frame", "V", v_trunc$summary)
  n_u <- nrow(u)
  n_v <- nrow(v)
  n_c <- nrow(get_coordinates(x))
  
  # print (would prefer to make this one print step)
  cat(
    biplot_descr,
    #n_u, " row", ifelse(n_u == 1, "", "s"), " and ",
    #n_v, " column", ifelse(n_v == 1, "", "s"), "\n",
    " ( ", n_u, " x ", n_c, " ) x ( ", n_v, " x ", n_c, " )'\n",
    "#   ", n_c, " coordinate", ifelse(n_c == 1, "", "s"), ": ",
    print_reps(get_coordinates(x)$.name), "\n",
    #if (wrapped) "" else paste0("#   Original class: '", prev_class, "'\n"),
    sep = ""
  )
  cat("# \n")
  print(u_trunc)
  cat("# \n")
  print(v_trunc)
  invisible(x)
}

print_reps <- function(x) {
  x <- as.character(x)
  switch(
    min(length(x), 4),
    "1" = x,
    "2" = paste(x, collapse = " and "),
    "3" = paste(x, collapse = ", "),
    "4" = paste0(paste(x[1:2], collapse = ", "), ", ..., ", x[length(x)])
  )
}
