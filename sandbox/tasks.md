# tidybiplot tasks

## object classes

- `svd`
- `prcomp` and [other PCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)
- `lda`
- `ca` and [other CA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/07/19/Correspondence-Analysis/)
- `mca` and [other MCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/)
- `NMF`
- [other matrix decomposition methods](http://scikit-learn.org/stable/modules/decomposition.html)

## methods

- `print.bbl`: Separate the matrices (by way of coordinates) from the annotation variables, maybe by a vertical line?

## verbs

### bibble to bibble

`project()`: Restrict to a subset of coordinates. Accept a single non-negative integer, coordinate indices, or coordinate names.

`amass()`/`consolidate()`: Replace `u` or `v` with centroids from a partition.

`transpose()`/`dualize()`: Interchange `u` and `v`.

`bind_u()` & `bind_v()`: Add new rows of `u` and/or `v` without changing the matrix transformations.

### two bibbles to one bibble

`bind()`: Augment the coordinates, and therefore both the `u`s and the `v`s, of two bibbles, leaving values of each zero at the other's coordinates (i.e. block matrices). _This functionality is of questionable utility and low-priority._

`align_to()`: Flip the signs of the coordinates to minimize the angle between the coordinates of an index bibble and those of an alter bibble or a suitable matrix object.

### bibble to tibble

`reconstruct()`: Multiply `u` and `t(v)` to recover or estimate the original data table.

## plot layers

Incorporate `inertia` argument to `fortify()` and/or to `stat_biplot()`.

`*_isoline()`: Ensure that isolines respect non-linear transformations.

Implement an option to calibrate, in the Greenacre sense.

## resources

- [Michael Greenacre, _Biplots in Practice_](http://www.multivariatestatistics.org/biplots.html)
- [loading plots on StackExchange](https://stats.stackexchange.com/a/119758/68743)
- [arrow positions on StackExchange](https://stats.stackexchange.com/a/141531/68743)
- [biplots on StackExchange](https://stats.stackexchange.com/a/141755/68743)
- [PCA before LDA on StackExchange](https://stats.stackexchange.com/a/109810/68743)
