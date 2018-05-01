# tidybiplot tasks

## object classes

`as_bibble()` and `to_bibble()` methods for the following biplot classes:
- `lm`, `mlm`, and `glm`
- `svd`
- `prcomp`, `princomp`, and [other PCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)
- multidimensional scaling (MDS) with `cmdscale()`
- `lda`
- `ca` and [other CA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/07/19/Correspondence-Analysis/)
- `mca` and [other MCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/)
- `NMF`
- [other matrix decomposition methods](http://scikit-learn.org/stable/modules/decomposition.html)

the `bbl` class:
- `as_bibble()` wraps a biplot object with the `bbl` class so that methods can extract basic information (matrix factors, coordinates) necessary for printing and visualization
- `to_bibble()` flattens a biplot object into a list of basic matrix factorization information from which the original object is unrecoverable

## formatting methods

`print.bbl`: Separate the matrices (by way of coordinates) from the annotation variables, maybe by a vertical line?

`plot.bbl`: `fortify() %>% plot()`?

## verbs

### bibble to vector

`pull_*()`: Extract an annotation from either $U$ or $V$.

### bibble to bibble

`select_*()`/`rename_*()`: Restrict to and/or rename annotation while keeping all coordinates.

`mutate_*()`/`transmute_*()`: Introduce a new annotation as a function of others, and optionally discard all others.

`project_*()`: Restrict to a subset of coordinates but keep annotation. Accept a single non-negative integer, coordinate indices, or coordinate names.

`group_by_*()`: As in **dplyr**, but don't allow grouping by coordinate variables.

`summarise_*()` or `amass_*()`: Replace $U$ or $V$ with centroids from a partition.

`transpose()`/`dualize()`: Interchange $U$ and $V$.

`rotate_*()`: Transform one factorization into another by a nonsingular rank-$r$ matrix.

### bibble and tibble to bibble

`regress_onto()`: ?

`bind_u()` & `bind_v()`: Add new rows of $U$ and/or $V$ without changing the matrix transformations. _This functionality is of questionable utility and low-priority._

### two bibbles to one bibble

`bind()`: Augment the coordinates, and therefore both the $U$s and the $V$s, of two bibbles, leaving values of each zero at the other's coordinates (i.e. block matrices). _This functionality is of questionable utility and low-priority._

`align_with()`: Reorder the coordinates and/or flip their signs, when consistent with the meaning imbued in the factorization, to minimize the angle between the coordinates of an index bibble and those of an alter bibble or suitable matrix object.

### bibble to tibble

`get_*()`/`factor_*()`: Extract the annotated tibble form or the unannotated matrix form of $U$ or $V$.

`reconstruct()`: Multiply $U$ and $V'$ to recover or estimate the original data table or _target matrix_.

### extend methods from flattened bibbles to wrapped bibbles

## plotting

Incorporate `inertia` argument to `fortify()` and/or to `stat_biplot()`. Carefully document and justify the choice of where along the "typical" workflow to calculate the inertia.

### plot layers

`*_isoline()`: Ensure that isolines respect non-linear transformations (e.g. `glm`).

### groupable plot layers

`*_centroid()`: Plot centroids as points, using the `group` aesthetic.

`*_ellipse()`: As in **ggbiplot**, using the `group` aesthetic.

### non-aesthetic plot layers

`*_unit_circle()`: Plot a unit circle.

Implement an option to calibrate, in the Greenacre sense.

## documentation

## testing

Include thorough **testthat** examples of all methods.

## performance

### C++ implementations
