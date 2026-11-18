# Plot and biplot methods for 'tbl_ord' objects

Adapt **stats** 'prcomp' and 'princomp' methods for
[`plot()`](https://rdrr.io/r/graphics/plot.default.html),
[`screeplot()`](https://rdrr.io/r/stats/screeplot.html), and
[`biplot()`](https://rdrr.io/r/stats/biplot.html) generics to 'tbl_ord'
objects.

## Usage

``` r
# S3 method for class 'tbl_ord'
plot(x, main = deparse(substitute(x)), ...)

# S3 method for class 'tbl_ord'
screeplot(x, main = deparse(substitute(x)), ...)

# S3 method for class 'tbl_ord'
biplot(x, main = deparse(substitute(x)), ...)
```

## Arguments

- x:

  A 'tbl_ord' object.

- main:

  A main title for the plot, passed to other methods (included to enable
  parsing of object name).

- ...:

  Additional arguments passed to other methods.

## Value

Nothing, but a plot is produced on the current graphics device.

## Details

These methods defer to any
[`plot()`](https://rdrr.io/r/graphics/plot.default.html) and
[`biplot()`](https://rdrr.io/r/stats/biplot.html) methods for the
original, underlying model classes of 'tbl_ord' objects. If none are
found: Following the examples of
[`stats::plot.prcomp()`](https://rdrr.io/r/stats/prcomp.html) and
[`stats::plot.princomp()`](https://rdrr.io/r/stats/princomp.html),
`plot.tbl_ord()` calls on
[`stats::screeplot()`](https://rdrr.io/r/stats/screeplot.html) to
produce a scree plot of the decomposition of variance in the singular
value decomposition. Similarly following
[`stats::biplot.prcomp()`](https://rdrr.io/r/stats/biplot.princomp.html)
and
[`stats::biplot.princomp()`](https://rdrr.io/r/stats/biplot.princomp.html),
`biplot.tbl_ord()` produces a biplot of both rows and columns, using
text labels when available and markers otherwise, with rows and columns
distinguished by color and no additional annotation (e.g. vectors). The
biplot confers inertia according to [`get_conference()`](conference.md)
unless the proportions do not sum to 1, in which case it produces a
symmetric biplot (inertia conferred equally to rows and columns).

## Examples

``` r
# note: behavior depends on installed packages with class-specific methods

# class 'prcomp'
iris_pca <- prcomp(iris[, -5L], scale = TRUE)
iris_pca_ord <- as_tbl_ord(iris_pca)
plot(iris_pca)

plot(iris_pca_ord)

screeplot(iris_pca)

screeplot(iris_pca_ord)

biplot(iris_pca)

biplot(iris_pca_ord)


# class 'correspondence'
haireye_ca <- MASS::corresp(rowSums(HairEyeColor, dims = 2L), nf = 2L)
haireye_ca_ord <- as_tbl_ord(haireye_ca)
plot(haireye_ca)

plot(haireye_ca_ord)

# no `screeplot()` method for class 'correspondence'
screeplot(haireye_ca_ord)

biplot(haireye_ca)

biplot(haireye_ca_ord)
```
