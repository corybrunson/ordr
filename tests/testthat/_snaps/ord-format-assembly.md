# ord_header snapshot full

    Code
      ord_header(layout)
    Output
      [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"
      [2] "# 4 coordinates: PC1, PC2, ..., PC4"                
      [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"          
      [4] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"            

# ord_header snapshot narrow

    Code
      ord_header(layout)
    Output
      [1] "# A tbl_ord <prc>: (150×4)·(4×4)´"   "# 4 coordinates: PC1, PC2, ..., PC4"
      [3] "# Rows (principal): [ 150 × 4 | 1 ]" "# Columns (standard): [ 4 × 4 | 3 ]"

# ord_header snapshot very narrow

    Code
      ord_header(layout)
    Output
      [1] "# A tbl_ord: (150×4)·(4×4)´" "# 4 coordinates"            
      [3] "# Rows: [150×4|1]"           "# Columns: [4×4|3]"         

# ord_header snapshot lda

    Code
      ord_header(layout)
    Output
      [1] "# A tbl_ord of class 'lda_ord': (153 × 2) · (4 × 2)´"
      [2] "# 2 coordinates: LD1 and LD2"                        
      [3] "# Rows (principal, 100%): [ 153 × 2 | 5 ]"           
      [4] "# Columns (standard, 0%): [ 4 × 2 | 2 ]"             

# footer snapshot

    Code
      strip_style(ord_footer(layout))
    Output
      [1] "# ℹ 145 more rows"                            
      [2] "# ℹ Use `print(n = ...)` to see more elements"

# footer snapshot all rows

    Code
      strip_style(ord_footer(layout))
    Output
      character(0)

# footer snapshot with max_extra_cols

    Code
      strip_style(ord_footer(layout))
    Output
      [1] "# ℹ 2 more variables:\n#   center <numeric>, .element <character>"

# footer snapshot narrow

    Code
      strip_style(ord_footer(layout))
    Output
      [1] "# ℹ 145 more rows"                            
      [2] "# ℹ Use `print(n = ...)` to see more elements"

# footer snapshot very narrow

    Code
      strip_style(ord_footer(layout))
    Output
      [1] "# ℹ 145 more rows"                            
      [2] "# ℹ Use `print(n = ...)` to see more elements"

# print snapshot stripped

    Code
      strip_style(format(ord_pca))
    Output
       [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"         
       [2] "# 4 coordinates: PC1, PC2, ..., PC4"                         
       [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"                   
       [4] "      PC1     PC2     PC3      PC4 | .element"               
       [5] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>   "               
       [6] "1 -2.68   -0.319   0.0279  0.00226 | active  "               
       [7] "2 -2.71    0.177   0.210   0.0990  | active  "               
       [8] "3 -2.89    0.145  -0.0179  0.0200  | active  "               
       [9] "4 -2.75    0.318  -0.0316 -0.0756  | active  "               
      [10] "5 -2.73   -0.327  -0.0901 -0.0613  | active  "               
      [11] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"                     
      [12] "      PC1     PC2     PC3      PC4 | name    center .element"
      [13] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>    <dbl> <chr>   "
      [14] "1  0.361  -0.657   0.582   0.315   | Sepal.~   5.84 active  "
      [15] "2 -0.0845 -0.730  -0.598  -0.320   | Sepal.~   3.06 active  "
      [16] "3  0.857   0.173  -0.0762 -0.480   | Petal.~   3.76 active  "
      [17] "4  0.358   0.0755 -0.546   0.754   | Petal.~   1.20 active  "
      [18] "# ℹ 145 more rows"                                           
      [19] "# ℹ Use `print(n = ...)` to see more elements"               

