
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
  
  
  
  # assemble plot matrix
  # ord_list <- matrix(data = list(), n_dims, n_dims)
  ord_list <- list()
  for (i in seq(n_dims)) for (j in seq(n_dims)) {
    
    if (i == j) {
      
      # fortified data frame
      rows_df <- fortify(ordination, .matrix = "rows")
      
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
        geom_histogram()
      
    } else {
      
      # p <- ggbiplot(iris_pca, aes(x = !! i, y = !! j),
      #               axis.type = "predictive", axis.percents = FALSE) +
      #   geom_rows_point(aes(color = Species, shape = Species)) +
      #   stat_rows_center(
      #     aes(color = Species, shape = Species),
      #     size = 5, alpha = .5, fun.data = mean_se
      #   ) +
      #   geom_cols_axis(aes(label = name, center = center, scale = scale)) +
      #   coord_cartesian()
      
    }
    
    # ord_list[[i, j]] <- p
    ord_list[[i + (j - 1) * n_dims]] <- p
    
  }
  iris_matrix <- GGally::ggmatrix(
    ord_list,
    4, 4,
    paste("PC", 1:4, sep = ""),
    paste("PC", 1:4, sep = ""),
    byrow = TRUE
  )
  iris_matrix
  
}
