# format default width snapshot

    Code
      format(ord_pca)
    Output
       [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"         
       [2] "# 4 coordinates: PC1, PC2, ..., PC4"                         
       [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"                   
       [4] "      PC1     PC2     PC3      PC4 | .element"               
       [5] "    [630] [36.16] [11.65]  [3.551] | <chr>   "               
       [6] "1 -2.68   -0.319   0.0279  0.00226 | active  "               
       [7] "2 -2.71    0.177   0.210   0.0990  | active  "               
       [8] "3 -2.89    0.145  -0.0179  0.0200  | active  "               
       [9] "4 -2.75    0.318  -0.0316 -0.0756  | active  "               
      [10] "5 -2.73   -0.327  -0.0901 -0.0613  | active  "               
      [11] "                ⋮                       ⋮    "               
      [12] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"                     
      [13] "      PC1     PC2     PC3      PC4 | name    center .element"
      [14] "      [1]     [1]     [1]      [1] | <chr>    <dbl> <chr>   "
      [15] "1  0.361  -0.657   0.582   0.315   | Sepal.~   5.84 active  "
      [16] "2 -0.0845 -0.730  -0.598  -0.320   | Sepal.~   3.06 active  "
      [17] "3  0.857   0.173  -0.0762 -0.480   | Petal.~   3.76 active  "
      [18] "4  0.358   0.0755 -0.546   0.754   | Petal.~   1.20 active  "
      [19] "# ℹ 145 more rows"                                           
      [20] "# ℹ Use `print(n = ...)` to see more elements"               

# format narrow width snapshot

    Code
      format(ord_pca, width = 30)
    Output
       [1] "# A tbl_ord: (150×4)·(4×4)´"                  
       [2] "# 4 coordinates"                              
       [3] "# Rows (100%): [ 150 × 4 | 1 ]"               
       [4] "      PC1 … | .element"                       
       [5] "    [630]   | <chr>   "                       
       [6] "1 -2.68     | active  "                       
       [7] "2 -2.71     | active  "                       
       [8] "3 -2.89   … | active  "                       
       [9] "4 -2.75     | active  "                       
      [10] "5 -2.73     | active  "                       
      [11] "    ⋮          ⋮    "                         
      [12] "# Columns (0%): [ 4 × 4 | 3 ]"                
      [13] "      PC1 … | name     "                      
      [14] "      [1]   | <chr>    "                      
      [15] "1  0.361    | Sepal.Le~"                      
      [16] "2 -0.0845 … | Sepal.Wi~"                      
      [17] "3  0.857    | Petal.Le~"                      
      [18] "4  0.358    | Petal.Wi~"                      
      [19] "# ℹ 145 more rows"                            
      [20] "# ℹ Use `print(n = ...)` to see more elements"

# format wide width snapshot

    Code
      format(ord_pca, width = 120)
    Output
       [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"              
       [2] "# 4 coordinates: PC1, PC2, ..., PC4"                              
       [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"                        
       [4] "      PC1     PC2     PC3      PC4 | .element"                    
       [5] "    [630] [36.16] [11.65]  [3.551] | <chr>   "                    
       [6] "1 -2.68   -0.319   0.0279  0.00226 | active  "                    
       [7] "2 -2.71    0.177   0.210   0.0990  | active  "                    
       [8] "3 -2.89    0.145  -0.0179  0.0200  | active  "                    
       [9] "4 -2.75    0.318  -0.0316 -0.0756  | active  "                    
      [10] "5 -2.73   -0.327  -0.0901 -0.0613  | active  "                    
      [11] "                ⋮                       ⋮    "                    
      [12] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"                          
      [13] "      PC1     PC2     PC3      PC4 | name         center .element"
      [14] "      [1]     [1]     [1]      [1] | <chr>         <dbl> <chr>   "
      [15] "1  0.361  -0.657   0.582   0.315   | Sepal.Length   5.84 active  "
      [16] "2 -0.0845 -0.730  -0.598  -0.320   | Sepal.Width    3.06 active  "
      [17] "3  0.857   0.173  -0.0762 -0.480   | Petal.Length   3.76 active  "
      [18] "4  0.358   0.0755 -0.546   0.754   | Petal.Width    1.20 active  "
      [19] "# ℹ 145 more rows"                                                
      [20] "# ℹ Use `print(n = ...)` to see more elements"                    

