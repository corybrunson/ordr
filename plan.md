# tidybiplot plan

## ordination functions

- Generalized ordination functions
    - Inspired by [the **parsnip** package](https://topepo.github.io/parsnip/articles/parsnip_Intro.html)
    - Either one for each mode (supervised and unsupervised) or one for each model type (SVD, PCA, MDS, NMF, etc.)
    - Specialize to implementations from various packages, e.g. `pca(data, engine = c("prcomp", "princomp", "psych", ...))`

A major and potentially valuable expansion of the package would be to implement generalized ordination functions, possibly two (for supervised and unsupervised methods) or possibly one for each model type (SVD, PCA, MDS, NMF, etc.), that specialize to implementations from various packages

## object classes

`as_bibble()` and `to_bibble()` methods for the following ordination classes:

- `lm`, `mlm`, and `glm`
- multidimensional scaling (MDS) with `cmdscale()`
- `svd`
- `prcomp`, `princomp`, `psych::principal()`, and [other PCA functions and classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)
- `lda`
- `ca` and [other CA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/07/19/Correspondence-Analysis/)
- functions (and classes?) from the **vegan** package: `cca()` and `rda()`; `metaMDS()`; `decorana()`
- `isoMDS()`
- `mca` and [other MCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/)
- `NMF` class from the **NMF** package
- [other matrix decomposition methods](http://scikit-learn.org/stable/modules/decomposition.html)

## coercion and attribution methods

The `"bbl"` class (may be changed to `"tbl_ord"`):

- `as_bibble()` wraps a biplot object with the `"bbl"` class so that methods can extract basic information (matrix factors, coordinates) necessary for printing and visualization
- As long as `ggplot()` uses `fortify()` internally to handle objects, this package should use `fortify.bbl()` to convert ordination objects into data frames for plotting.

Coercion to class `"bbl"` comes with the following methods:

- `recover_factor()` (`recover_u()` & `recover_v()`): Extract either or both matrix factors comprising the ordination object.
- `recover_coord()`: Extract the names of the model/latent coordinates shared by the matrix factors.
- `augment_factor()` (`augment_u()` & `augment_v()`): Extract either or both case- and variable-level attributes, other than the coordinates, from the ordination object, as tibbles. For example: names (both), residuals from a linear regression model (case-level) or the amount of variance explained by each principal component in a PCA (variable-level).
- `augment_coord()`: Extract a tibble containing any attributes of the shared coordinates, for example the eigenvalues associated with the dimensions of an MDS.

These methods are the foundation for other bibble functions:

- `print()`: Print a concise summary of matrix factors and their attributes similar to the method for class `"tbl"`; uses `recover_factor()`, `augment_factor()`, and `recover_coord()`
- `fortify()`: Produce a tibble for either or both matrix factors, including their coordinates and optionally shared or all attributes, for use in rectangular data processing or plotting.
- `plot()` and `biplot()`: Produce a base R biplot of the cases and variables on their shared coordinates, unless a method already exists for the original ordination class.
- `ggbiplot()`: Produce a two-dimensional biplot of the ordination object, using the first two shared coordinates by default (`aes(x = 1, y = 2)`) but allowing specification to any shared coordinates.
- `predict()`: Ordinate new cases observed along the original variables.
- `supplement()`: Ordinate new variables measured for the original cases.
- `reconstruct()`: Approximate the original data table, consisting of estimated values of each variable for each case, by multiplying the matrix factors and invoking any transformations remembered by the ordination object. This method will use `recover_factor()` and may encode the formula in the method or as a formula object in the bibble (e.g. in case some ordination methods have dropped parameters).

## verbs

### transform coordinates

It might be appropriate to have a `transpose()` or `dualize()` function to interchange $U$ and $V$.

Several functions transform the coordinates _without changing the structure of the original ordination object_, other than to add or edit a `"alignment"` attribute that contains the transformation matrix:

- `transform()`: Transform one factorization into another by matrix multiplication on the coordinates. The functions below are special cases.
- `negate()`: Negate specified coordinates. Accepts a subset of `1:dim(x)` or a $1,-1$-vector of `1:dim(x)`.
- `permute()`: Permute coordinates. Accepts a permutation of `1:dim(x)` or a permutation matrix.
- `rotate()`: Rotate $U$ and $V$. Accepts any non-singular matrix and produces a rotation matrix by orthonormalizing each vector with respect to the previous.
- `project_to()`: Restrict to a subset of coordinates. Accepts a vector of integer coordinate indices or character coordinate names.
- `align_to()`: Reorder the coordinates and/or flip their signs, when consistent with the meaning imbued in the factorization, to minimize the angle between the coordinates of an index bibble and those of an alter bibble or suitable matrix object.

- `regress_onto()`: ?

Create functions to calculate individual observations' or variables' contributions to error (SSE, stress, etc.), outlier/inlier status, or other atypicalities.

### **dplyr** verbs to manipulate annotation

These functions add or manipulate case, variable, and coordinate annotations:

- `pull_*()`: Extract an annotation from either $U$ or $V$.
- `select_*()`/`rename_*()`: Restrict to and/or rename annotation.
- `mutate_*()`/`transmute_*()`: Introduce a new annotation as a function of others and of coordinates, and optionally discard existing annotation.

_Note: **dplyr**-style verbs like `filter()`, `summarise()`, and `bind_*()` would affect the coordinates as well as the annotation. This functionality is of questionable utility and low-priority._

## plotting

Incorporate `inertia` argument to `as_bibble()`, `fortify()` and/or `stat_biplot()` (see also the `expand` parameter of `stats::biplot()`). Carefully document and justify the choice of where along the "typical" workflow to calculate the inertia.

## other plot specifications

### scales and coordinates

- By default, breaks should be the same on the horizontal and vertical axes. This might require scale layers not constructible via `continuous_scale()`; see `?ggplot2::Scale`.
- An `inertia` parameter could control the scales of $U$ and $V$. If used, it should render, by default, breaks based on one matrix factor with labels on the bottom and left axes, but also labels on the top and right axes that reflect the scale of the other factor.

### faceting

- `facet_*_wrap()` and `facet_*_grid()` to facet as in **ggplot2** with respect to $U$ attributes.
- `facet_uv_grid()` and `facet_vu_grid()` to facet with respect to an attribute of $U$ in one dimension and an attribute of $V$ in the other dimension.

## plot layers

- `stat_u()` & `stat_v()`: Restrict to cases ($U$) or variables ($V$).

### non-case/variable plot layers

- `geom_unit_circle()`: Plot a unit circle.

### case/variable layers

- `geom_*_point()`: Render symbols at the positions of cases and variables on the biplot.
- `geom_*_vector()` and `geom_*_line(): Render vectors with tails at the origin and heads at the case/variable positions.
- `geom_*_projection()`: Render projections from specific cases to specific vectors.
- `geom_*_text()` and `geom_*_label()`: Render text at case/variable positions. Include an option to angle them so that they eminate from the origin after arrowheads, as in **ggbiplot**.

### single-case/variable layers

- `geom_*_isoline()`: Render isolines for a specific variable. Ensure that isolines respect non-linear transformations (e.g. `glm`).

### grouped-case/variable layers

- `stat_*_centroid()`: Calculate group centroids.
- `stat_*_chull()` & `geom_*_chull()`: Calculate and render group convex hulls.
- `stat_*_ellipse()` & `geom_*_ellipse()`: Calculate and render group ellipses, as in **ggbiplot**. Includes confidence ellipses, standard error ellipses, and ellipsoid hulls.

### extra

Implement an option to _calibrate_, in the Greenacre sense.

## documentation

### summary information

Include keywords in the description: "biplot", "(linear) ordination" ([helpfully defined by János Podani](http://ramet.elte.hu/~podani/7-Ordination.pdf)), "geometric data analysis".
Consider these when deciding on a final name. Candidates: **tidybiplot**, **ordin8r**, **ordy**, **orb**

### theory

Conceptualize each technique (and each of its variants) as a flow chart beginning with the raw data and progressing through each transformation, including the matrix factorization (with its objective function), ending with the two matrices from which the biplot is constructed.

Identify the calculations (e.g. projections) and visual elements (e.g. ellipses) that are appropriate to the biplots obtained using each technique.

### motivation

What is missing from related packages for tidy ordination and biplot that this package provides?

- [**factoextra**](http://www.sthda.com/english/rpkgs/factoextra/)
- [**ggfortify**](https://journal.r-project.org/archive/2016/RJ-2016-060/index.html)
- [**ggbiplot**](https://github.com/GegznaV/ggbiplot)
- [**ggord**](https://beckmw.wordpress.com/2015/05/14/reinventing-the-wheel-for-ordination-biplots-with-ggplot2/)
- [**phyloseq**](https://joey711.github.io/phyloseq/plot_ordination-examples.html)

## testing

Include thorough **testthat** examples of all methods.

## performance
