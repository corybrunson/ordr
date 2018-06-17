# tidybiplot tasks

## augmentation

functions to augment the output of an ordination function to the original dataset

## object classes

`as_bibble()` and `to_bibble()` methods for the following biplot classes:
- `lm`, `mlm`, and `glm`
- `isoMDS()`
- `svd`
- `prcomp`, `princomp`, `psych::principal()`, and [other PCA functions and classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/06/17/PCA-in-R/)
- multidimensional scaling (MDS) with `cmdscale()`
- `lda`
- `ca` and [other CA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/07/19/Correspondence-Analysis/)
- functions (and classes?) from the **vegan** package: `cca()` and `rda()`; `metaMDS()`; `decorana()`
- `mca` and [other MCA classes](http://www.gastonsanchez.com/visually-enforced/how-to/2012/10/13/MCA-in-R/)
- `NMF`
- [other matrix decomposition methods](http://scikit-learn.org/stable/modules/decomposition.html)

## coercion and attribution methods

the `"bbl"` class:
- `as_bibble()` wraps a biplot object with the `"bbl"` class so that methods can extract basic information (matrix factors, coordinates) necessary for printing and visualization
- As long as `ggplot()` uses `fortify()` internally to handle objects, this package should use `fortify.bbl()` to convert ordination objects into data frames for plotting.

Coercion to class `"bbl"`:
- Comes with the following methods:
  - `get_factor()` (`get_u()` & `get_v()`): Extract either or both matrix factors comprising the ordination object.
  - `get_coord()`: Extract the names of the model/latent coordinates shared by the matrix factors.
  - `factor_annot()` (`u_annot()` & `v_annot()`): Extract either or both subject- and variable-level attributes, other than the coordinates, from the ordination object, as tibbles. For example: names (both), residuals from a linear regression model (subject-level) or the amount of variance explained by each principal component in a PCA (variable-level).
  - `coord_annot()`: Extract a tibble containing any attributes of the shared coordinates, for example the eigenvalues associated with the dimensions of an MDS.
  - `print()`: Print a concise summary of matrix factors and their attributes similar to the method for class `"tbl"`; uses `get_factor()`, `factor_annot()`, and `get_coord()`
  - `fortify()`: Produce a tibble for either or both matrix factors, including their coordinates and optionally shared or all attributes, for use in rectangular data processing or plotting.
  - `plot()` and `biplot()`: Produce a base R biplot of the subjects and variables on their shared coordinates, unless a method already exists for the original ordination class.
  - `ggbiplot()`: Produce a two-dimensional biplot of the ordination object, using the first two shared coordinates by default (`aes(x = 1, y = 2)`) but allowing specification to any shared coordinates.
  - `predict()`: Ordinate new subjects observed along the original variables.
  - `supplement()`: Ordinate new variables measured for the original subjects.
  - `reconstruct()`: Approximate the original data table by multiplying the matrix factors and invoking any transformations remembered by the ordination object; uses `get_factor()`. May encode the formula in the method or as a formula object in the bibble (e.g. in case some ordination methods have dropped parameters).
- Allows additional attributes to be augmented to the matrix factors as tibble attributes (`"annot_u"` and `"annot_v"`) using adapted **dplyr** verbs.
- Has methods to estimate projections of scores on loadings or vice-versa.

## verbs

### structural verbs

`transpose()`/`dualize()`: Interchange $U$ and $V$.

`rotate_by()`: Transform one factorization into another by a nonsingular rank-$r$ matrix.

Functions to calculate individual observations' or variables' contributions to error (SSE, stress, etc.), outlier/inlier status, or other atypicalities.

`regress_onto()`: ?

`align_with()`: Reorder the coordinates and/or flip their signs, when consistent with the meaning imbued in the factorization, to minimize the angle between the coordinates of an index bibble and those of an alter bibble or suitable matrix object.

### **dplyr** verbs to update annotation

_This functionality is of questionable utility and low-priority._

`pull_*()`: Extract an annotation from either $U$ or $V$.

`select_*()`/`rename_*()`: Restrict to and/or rename annotation while keeping all coordinates.

`mutate_*()`/`transmute_*()`: Introduce a new annotation as a function of others, and optionally discard all others.

`project_*()`: Restrict to a subset of coordinates but keep annotation. Accept a single non-negative integer, coordinate indices, or coordinate names.

`group_by_*()`: As in **dplyr**, but don't allow grouping by coordinate variables.

`summarise_*()` or `amass_*()`: Replace $U$ or $V$ with centroids from a partition.

`bind_u()` & `bind_v()`: Add new rows of $U$ and/or $V$ without changing the matrix transformations.

`bind()`: Augment the coordinates, and therefore both the $U$s and the $V$s, of two bibbles, leaving values of each zero at the other's coordinates (i.e. block matrices).

## plotting

Incorporate `inertia` argument to `as_bibble()`, `fortify()` and/or `stat_biplot()` (see also the `expand` parameter of `stats::biplot()`). Carefully document and justify the choice of where along the "typical" workflow to calculate the inertia.

Develop a generalized scree plot (`ggord() + geom_scree()`?) with options (variance explained, stress, etc.) appropriate to classes.

## plot layers

### non-variable plot layers

`*_unit_circle()`: Plot a unit circle.

### variable layers

`*_*_text()` and `*_*_label()`: Option to align text with vectors, as in **ggbiplot**.

### single-row layers

`*_*_isoline()`: Draw isolines for a specific variable. Ensure that isolines respect non-linear transformations (e.g. `glm`).

### group layers

`*_*_centroid()`: Plot centroids as points, using the `group` aesthetic.

`*_*_ellipse()`: As in **ggbiplot**, using the `group` aesthetic. Include confidence ellipses, standard error ellipses, and ellipsoid hulls.

Implement an option to calibrate, in the Greenacre sense.

## documentation

### summary information

Include keywords in the description: "biplot", "(linear) ordination" ([helpfully defined by János Podani](http://ramet.elte.hu/~podani/7-Ordination.pdf)), "geometric data analysis".
Consider these when deciding on a final name. Candidates: **tidybiplot**, **ordin8r**, **ordy**, **orb**

### theory

Conceptualize each technique (and each of its variants) as a flow chart beginning with the raw data and progressing through each transformation, including the matrix factorization (with its objective function), ending with the two matrices from which the biplot is constructed.

Identify the calculations (e.g. projections) and visual elements (e.g. ellipses) that are appropriate to the biplots obtained using each technique.

## testing

Include thorough **testthat** examples of all methods.

## performance
