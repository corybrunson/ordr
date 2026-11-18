# Scaffolding theme

Omit cartesian coordinate visual aids.

## Usage

``` r
theme_scaffold()

theme_biplot()
```

## Value

A ggplot [theme](https://ggplot2.tidyverse.org/reference/theme.html).

## Details

Geometric data analysis concerns the intrinsic geometry of data.
Analyses often use artificial or arbitrary coordinate systems that carry
no useful interpretation but instead serve as scaffolding, especially
for graphical elements like
[axes](https://corybrunson.github.io/gggda/reference/geom_axis.html)
that represent other variables (Gardner, 2001). In such cases, the
visual aids (tick marks and labels, grid lines) used to recover the
coordinates of the row and column markers would add unnecessary clutter
and should be omitted. This partial theme updates the current theme by
removing these elements. The biplot theme is an alias included for
convenience and backward compatibility.

## References

Gardner S (2001) *Extensions of biplot methodology to discriminant
analysis with applications of non-parametric principal components*. PhD
thesis, Stellenbosch University.
<https://scholar.sun.ac.za/items/279f7958-0b54-43f1-8c75-da652f65db3f>
