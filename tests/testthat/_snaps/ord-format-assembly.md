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
      [3] "# Rows: 150 × 4"             "# Columns: 4 × 4"           

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

