data(issp_women)
(m <- ca::ca(issp_women))
(b <- as_tbl_ord(m))

b %>% augment() %>% augment()
# BUGS!!
b %>%
  fortify(matrix = "u")
b %>%
  augment() %>%
  fortify(matrix = "u")
b %>%
  #augment() %>%
  fortify_u()
# SOLUTIONS
# augmentation_*() = `tbl_df` of row-wise model stats without coordinates
# augment_*() = `tbl_ord` with `augmentation_*()` attributed
# annotate_*() = know to avoid `augment_*()` column names?
# fortify() = `tbl_df` of coordinates and all annotation
# REVISIONS (to address elsewhere)
# augment_u, augment_v, augment_coord, annotate_u, annotate_v

(pca <- as_tbl_ord(prcomp(USPersonalExpenditure, center = FALSE)))
pca %>% augment()
pca %>% augment() %>% augment()
pca %>% fortify(matrix = "u")
pca %>% augment() %>% fortify(matrix = "u")
