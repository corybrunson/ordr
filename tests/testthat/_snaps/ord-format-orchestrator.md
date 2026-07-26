# format default width snapshot

    Code
      format(ord_pca)
    Output
       [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"       
       [2] "# 4 coordinates: PC1, PC2, ..., PC4"                       
       [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"                 
       [4] "      PC1     PC2     PC3    PC4 | .element"               
       [5] "    <dbl>   <dbl>   <dbl>  <dbl> | <chr>   "               
       [6] "1 -2.68 -0.319  0.0279  0.00226  | active  "               
       [7] "2 -2.71  0.177  0.210   0.0990   | active  "               
       [8] "3 -2.89  0.145 -0.0179  0.0200   | active  "               
       [9] "4 -2.75  0.318 -0.0316 -0.0756   | active  "               
      [10] "5 -2.73 -0.327 -0.0901 -0.0613   | active  "               
      [11] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"                   
      [12] "      PC1     PC2     PC3    PC4 | name    center .element"
      [13] "    <dbl>   <dbl>   <dbl>  <dbl> | <chr>    <dbl> <chr>   "
      [14] "1  0.361  -0.657   0.582   0.315 | Sepal.~   5.84 active  "
      [15] "2 -0.0845 -0.730  -0.598  -0.320 | Sepal.~   3.06 active  "
      [16] "3  0.857   0.173  -0.0762 -0.480 | Petal.~   3.76 active  "
      [17] "4  0.358   0.0755 -0.546   0.754 | Petal.~   1.20 active  "
      [18] "# ℹ 145 more rows"                                         
      [19] "# ℹ Use `print(n = ...)` to see more elements"             

# format narrow width snapshot

    Code
      format(ord_pca, width = 30)
    Output
       [1] "# A tbl_ord: (150×4)·(4×4)´"                  
       [2] "# 4 coordinates"                              
       [3] "# Rows (100%): [ 150 × 4 | 1 ]"               
       [4] "      PC1 … | .element"                       
       [5] "    <dbl>   | <chr>   "                       
       [6] "1 -2.68     | active  "                       
       [7] "2 -2.71     | active  "                       
       [8] "3 -2.89   … | active  "                       
       [9] "4 -2.75     | active  "                       
      [10] "5 -2.73     | active  "                       
      [11] "# Columns (0%): [ 4 × 4 | 3 ]"                
      [12] "      PC1 … | name     "                      
      [13] "    <dbl>   | <chr>    "                      
      [14] "1  0.361    | Sepal.Le~"                      
      [15] "2 -0.0845 … | Sepal.Wi~"                      
      [16] "3  0.857    | Petal.Le~"                      
      [17] "4  0.358    | Petal.Wi~"                      
      [18] "# ℹ 145 more rows"                            
      [19] "# ℹ Use `print(n = ...)` to see more elements"

# format wide width snapshot

    Code
      format(ord_pca, width = 120)
    Output
       [1] "# A tbl_ord of class 'prcomp': (150 × 4) · (4 × 4)´"            
       [2] "# 4 coordinates: PC1, PC2, ..., PC4"                            
       [3] "# Rows (principal, 100%): [ 150 × 4 | 1 ]"                      
       [4] "      PC1     PC2     PC3    PC4 | .element"                    
       [5] "    <dbl>   <dbl>   <dbl>  <dbl> | <chr>   "                    
       [6] "1 -2.68 -0.319  0.0279  0.00226  | active  "                    
       [7] "2 -2.71  0.177  0.210   0.0990   | active  "                    
       [8] "3 -2.89  0.145 -0.0179  0.0200   | active  "                    
       [9] "4 -2.75  0.318 -0.0316 -0.0756   | active  "                    
      [10] "5 -2.73 -0.327 -0.0901 -0.0613   | active  "                    
      [11] "# Columns (standard, 0%): [ 4 × 4 | 3 ]"                        
      [12] "      PC1     PC2     PC3    PC4 | name         center .element"
      [13] "    <dbl>   <dbl>   <dbl>  <dbl> | <chr>         <dbl> <chr>   "
      [14] "1  0.361  -0.657   0.582   0.315 | Sepal.Length   5.84 active  "
      [15] "2 -0.0845 -0.730  -0.598  -0.320 | Sepal.Width    3.06 active  "
      [16] "3  0.857   0.173  -0.0762 -0.480 | Petal.Length   3.76 active  "
      [17] "4  0.358   0.0755 -0.546   0.754 | Petal.Width    1.20 active  "
      [18] "# ℹ 145 more rows"                                              
      [19] "# ℹ Use `print(n = ...)` to see more elements"                  

