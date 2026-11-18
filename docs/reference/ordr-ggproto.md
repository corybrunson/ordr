# ggproto classes created and adapted for ordr

In addition to geometric element layers (geoms) based on
base-**ggplot2** layers like
[`geom_point()`](https://ggplot2.tidyverse.org/reference/geom_point.html)
but specified to matrix factors as `geom_row_point()`, **ordr**
introduces
[ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html) classes
for some additional geometric elements commonly used in biplots. The
factor-specific geoms invoke the statistical transformation layers
(stats) [`stat_rows()`](stat_rows.md) and [`stat_cols()`](stat_rows.md),
which specify the matrix factor. Because each ggplot layer consists of
only one stat and one geom, this necessitates that ggproto classes for
new stats must also come in `*Rows` and `*Cols` flavors.

## See also

[`ggplot2::ggplot2-ggproto`](https://ggplot2.tidyverse.org/reference/ggplot2-ggproto.html)
and
[ggplot2::ggproto](https://ggplot2.tidyverse.org/reference/ggproto.html)
for explanations of base ggproto classes in **ggplot2** and how to
create new ones.
