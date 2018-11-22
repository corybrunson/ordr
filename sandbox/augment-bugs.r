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
# 


(pca <- as_tbl_ord(prcomp(USPersonalExpenditure, center = FALSE)))
pca %>% augment() %>% augment()
pca %>% augment() %>% fortify(matrix = "u")
