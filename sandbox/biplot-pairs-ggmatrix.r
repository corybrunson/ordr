
ordination = iris_pca
mapping = aes(color = Species, fill = Species, linetype = name)
coords = seq(3)

ggbipairs <- function(
    ordination, mapping = NULL, coords = NULL,
    ...,
    upper = list(rows = "point", cols = "vector"),
    lower = list(rows = "point", cols = "vector"),
    diag = list(rows = "histogram", cols = "blank")
) {
  
  if (any(c("x", "y") %in% names(mapping))) {
    warning("`x` or `y` was passed to a mapping aesthetic and will be ignored.")
  }
  
  # all ordination dimensions
  dims <- recover_coord(ordination)
  # restrict to explicitly specified dimensions, if any
  if (! is.null(coords)) {
    if (is.numeric(coords)) {
      dims <- dims[intersect(seq_along(dims), coords)]
    } else if (is.character(coords)) {
      dims <- dims[match(coords, dims)]
    } else {
      warning("`coords` is neither numeric nor character and will be ignored.")
    }
  }
  n_dims <- length(dims)
  
  # fortified data frames
  dims_df <- fortify(ordination, .matrix = "both")
  rows_df <- dplyr::filter(dims_df, .matrix == "rows")
  cols_df <- dplyr::filter(dims_df, .matrix == "cols")
  
  # all dimension limits
  lims <- sapply(dims_df[dims], range, na.rm = TRUE, simplify = TRUE)
  
  # create matrix plots
  ord_list <- list()
  for (i in seq(n_dims)) for (j in seq(n_dims)) {
    
    if (i == j) {
      
      # combine aesthetics
      section_aes <- aes(x = !! as.name(dims[[i]]))
      for (k in seq_along(mapping)) section_aes[names(mapping)[k]] <- mapping[k]
      # remove missing aesthetics
      match_aes <- match(
        as.character(sapply(section_aes, rlang::quo_get_expr)),
        names(rows_df)
      )
      for (k in seq_along(match_aes)) 
        if (is.na(match_aes[k])) section_aes[k] <- NULL
      
      # 1-dimensional biplot
      p <- ggplot(rows_df, section_aes) +
        geom_histogram() +
        scale_x_continuous(limits = lims[, i]) +
        theme(aspect.ratio = 1)
      
    } else {
      
      # combine aesthetics
      section_aes <- aes(x = !! as.name(dims[[i]]), y = !! as.name(dims[[j]]))
      for (k in seq_along(mapping)) section_aes[names(mapping)[k]] <- mapping[k]
      # remove missing aesthetics
      match_aes <- match(
        as.character(sapply(section_aes, rlang::quo_get_expr)),
        names(dims_df)
      )
      for (k in seq_along(match_aes)) 
        if (is.na(match_aes[k])) section_aes[k] <- NULL
      
      p <- ggbiplot(dims_df, section_aes) +
        geom_rows_point() +
        stat_rows_center(size = 5, alpha = .5, fun.data = mean_se) +
        geom_cols_axis() +
        scale_x_continuous(limits = lims[, i]) +
        scale_y_continuous(limits = lims[, j])
      
    }
    
    ord_list[[i + (j - 1) * n_dims]] <- p
    
  }
  
  # assemble plot matrix
  iris_matrix <- GGally::ggmatrix(
    ord_list,
    n_dims, n_dims,
    dims, dims,
    byrow = TRUE
  )
  
  iris_matrix
  
}

ggbipairs(
  iris_pca, aes(color = Species, fill = Species, linetype = name), coords = 1:3
)
