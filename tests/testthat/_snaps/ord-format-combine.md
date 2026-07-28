# ord_combine snapshot default

    Code
      combined
    Output
      $rows
      [1] "      PC1     PC2     PC3      PC4 | .element"
      [2] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>   "
      [3] "1 -2.68   -0.319   0.0279  0.00226 | active  "
      [4] "2 -2.71    0.177   0.210   0.0990  | active  "
      [5] "3 -2.89    0.145  -0.0179  0.0200  | active  "
      [6] "4 -2.75    0.318  -0.0316 -0.0756  | active  "
      [7] "5 -2.73   -0.327  -0.0901 -0.0613  | active  "
      
      $cols
      [1] "      PC1     PC2     PC3      PC4 | name    center .element"
      [2] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>    <dbl> <chr>   "
      [3] "1  0.361  -0.657   0.582   0.315   | Sepal.~   5.84 active  "
      [4] "2 -0.0845 -0.730  -0.598  -0.320   | Sepal.~   3.06 active  "
      [5] "3  0.857   0.173  -0.0762 -0.480   | Petal.~   3.76 active  "
      [6] "4  0.358   0.0755 -0.546   0.754   | Petal.~   1.20 active  "
      

# ord_combine snapshot narrow

    Code
      combined
    Output
      $rows
      [1] "      PC1 … | .element" "    <dbl>   | <chr>   " "1 -2.68     | active  "
      [4] "2 -2.71     | active  " "3 -2.89   … | active  " "4 -2.75     | active  "
      [7] "5 -2.73     | active  "
      
      $cols
      [1] "      PC1 … | name     " "    <dbl>   | <chr>    "
      [3] "1  0.361    | Sepal.Le~" "2 -0.0845 … | Sepal.Wi~"
      [5] "3  0.857    | Petal.Le~" "4  0.358    | Petal.Wi~"
      

# ord_combine snapshot wide

    Code
      combined
    Output
      $rows
      [1] "      PC1     PC2     PC3      PC4 | .element"
      [2] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>   "
      [3] "1 -2.68   -0.319   0.0279  0.00226 | active  "
      [4] "2 -2.71    0.177   0.210   0.0990  | active  "
      [5] "3 -2.89    0.145  -0.0179  0.0200  | active  "
      [6] "4 -2.75    0.318  -0.0316 -0.0756  | active  "
      [7] "5 -2.73   -0.327  -0.0901 -0.0613  | active  "
      
      $cols
      [1] "      PC1     PC2     PC3      PC4 | name         center .element"
      [2] "    <dbl>   <dbl>   <dbl>    <dbl> | <chr>         <dbl> <chr>   "
      [3] "1  0.361  -0.657   0.582   0.315   | Sepal.Length   5.84 active  "
      [4] "2 -0.0845 -0.730  -0.598  -0.320   | Sepal.Width    3.06 active  "
      [5] "3  0.857   0.173  -0.0762 -0.480   | Petal.Length   3.76 active  "
      [6] "4  0.358   0.0755 -0.546   0.754   | Petal.Width    1.20 active  "
      

