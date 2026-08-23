# Package index

## ‘ordr’ package

A unified framework for ordination objects and biplot methods using tidy
principles

- [`ordr`](ordr.md) [`ordr-package`](ordr.md) :

  **ordr** package

## Functions, classes, and methods

Introducing and adapting ordination techniques for the **ordr** workflow

- [`lda_ord()`](lda-ord.md) [`predict(`*`<lda_ord>`*`)`](lda-ord.md)
  [`model.frame(`*`<lda_ord>`*`)`](lda-ord.md) : Augmented
  implementation of linear discriminant analysis
- [`lra()`](lra-ord.md) [`print(`*`<lra>`*`)`](lra-ord.md)
  [`screeplot(`*`<lra>`*`)`](lra-ord.md)
  [`biplot(`*`<lra>`*`)`](lra-ord.md) [`plot(`*`<lra>`*`)`](lra-ord.md)
  : Log-ratio analysis
- [`eigen_ord()`](wrap-ord.md) [`svd_ord()`](wrap-ord.md)
  [`cmdscale_ord()`](wrap-ord.md) [`cancor_ord()`](wrap-ord.md) :
  Wrappers for lossy ordination methods

## Data sets

Additional data sets used to illustrate ordination techniques

- [`glass`](glass.md) : Glass composition data
- [`qswur_usa`](qswur_usa.md) : U.S. university rankings

## Tidy ordination

The ‘tbl_ord’ class wrapper, with methods for accessing components and
for querying and manipulating metadata

- [`recover_rows()`](recoverers.md) [`recover_cols()`](recoverers.md)
  [`get_rows()`](recoverers.md) [`get_cols()`](recoverers.md)
  [`as.matrix(`*`<tbl_ord>`*`)`](recoverers.md)
  [`recover_inertia()`](recoverers.md)
  [`recover_coord()`](recoverers.md) [`get_coord()`](recoverers.md)
  [`get_inertia()`](recoverers.md)
  [`dim(`*`<tbl_ord>`*`)`](recoverers.md) : Access factors, coordinates,
  and metadata from ordination objects

- [`recover_aug_rows()`](augmentation.md)
  [`recover_aug_cols()`](augmentation.md)
  [`recover_aug_coord()`](augmentation.md)
  [`augment_ord()`](augmentation.md) : Augment factors and coordinates
  of 'tbl_ord' objects

- [`tidy(`*`<tbl_ord>`*`)`](tidiers.md)
  [`glance(`*`<tbl_ord>`*`)`](tidiers.md)
  [`fortify(`*`<tbl_ord>`*`)`](tidiers.md) : Tidiers for 'tbl_ord'
  objects

- [`annotation`](annotation.md) : Annotate factors of 'tbl_ord' objects

- [`get_negation()`](negation.md) [`revert_negation()`](negation.md)
  [`negate_ord()`](negation.md)
  [`negate_to_first_orthant()`](negation.md) : Negation of ordination
  axes

- [`recover_conference()`](conference.md)
  [`get_conference()`](conference.md)
  [`revert_conference()`](conference.md)
  [`confer_inertia()`](conference.md) : Confer inertia to factors of a
  'tbl_ord' object

- [`recover_supp_rows()`](supplementation.md)
  [`recover_supp_cols()`](supplementation.md) : Supplement 'tbl_ord'
  objects with new data

- [`as_tbl_ord()`](tbl_ord.md) [`make_tbl_ord()`](tbl_ord.md)
  [`is_tbl_ord()`](tbl_ord.md) [`is.tbl_ord()`](tbl_ord.md)
  [`valid_tbl_ord()`](tbl_ord.md) [`un_tbl_ord()`](tbl_ord.md) : A
  unified ordination object class

- [`format(`*`<tbl_ord>`*`)`](format.md)
  [`print(`*`<tbl_ord>`*`)`](format.md) : Format a tbl_ord for printing

- [`ord_quality()`](goodness-of-fit.md)
  [`ord_adequacy()`](goodness-of-fit.md)
  [`ord_predictivity()`](goodness-of-fit.md) : Measures of goodness of
  fit of ordination models

- [`plot(`*`<tbl_ord>`*`)`](plot.tbl_ord.md)
  [`screeplot(`*`<tbl_ord>`*`)`](plot.tbl_ord.md)
  [`biplot(`*`<tbl_ord>`*`)`](plot.tbl_ord.md) : Plot and biplot methods
  for 'tbl_ord' objects

- [`pull_factor()`](dplyr-verbs.md) [`pull_rows()`](dplyr-verbs.md)
  [`pull_cols()`](dplyr-verbs.md) [`rename_rows()`](dplyr-verbs.md)
  [`rename_cols()`](dplyr-verbs.md) [`select_rows()`](dplyr-verbs.md)
  [`select_cols()`](dplyr-verbs.md) [`mutate_rows()`](dplyr-verbs.md)
  [`mutate_cols()`](dplyr-verbs.md) [`transmute_rows()`](dplyr-verbs.md)
  [`transmute_cols()`](dplyr-verbs.md) [`cbind_rows()`](dplyr-verbs.md)
  [`cbind_cols()`](dplyr-verbs.md) [`left_join_rows()`](dplyr-verbs.md)
  [`left_join_cols()`](dplyr-verbs.md) :

  **dplyr** verbs for tbl_ord factors

- [`ordinate()`](ordinate.md) : Fit an ordination model to a data object

## Layered grammar for biplots

Functions to generate and annotate biplots from ‘tbl_ord’ objects,
following the layered grammar of graphics

- [`ggbiplot()`](ggbiplot.md) [`ord_aes()`](ggbiplot.md) : Biplots
  following the grammar of graphics
- [`coord_scaffold()`](coord_scaffold.md) : Convenience coordinate
  system for scaffolding axes
- [`stat_rows()`](stat_rows.md) [`stat_cols()`](stat_rows.md) : Render
  plot elements for one matrix of an ordination
- [`stat_projection()`](stat_projection.md) : Project rows onto columns
  or vice-versa
- [`geom_origin()`](geom_origin.md)
  [`geom_unit_circle()`](geom_origin.md) : Marker or unit circle at the
  origin
- [`geom_interpolation()`](geom_interpolation.md) : Render interpolation
  of new rows from columns (or vice-versa)
- [`theme_scaffold()`](theme_scaffold.md)
  [`theme_biplot()`](theme_scaffold.md) : Scaffolding theme
- [`draw_key_line()`](draw-key.md)
  [`draw_key_crosslines()`](draw-key.md)
  [`draw_key_crosspoint()`](draw-key.md) : Biplot key drawing functions
- [`ordr-ggproto`](ordr-ggproto.md) [`CoordScaffold`](ordr-ggproto.md)
  [`GeomInterpolation`](ordr-ggproto.md) [`GeomOrigin`](ordr-ggproto.md)
  [`GeomUnitCircle`](ordr-ggproto.md) [`StatRows`](ordr-ggproto.md)
  [`StatCols`](ordr-ggproto.md) [`StatProjection`](ordr-ggproto.md)
  [`StatRowsDensity2d`](ordr-ggproto.md)
  [`StatColsDensity2d`](ordr-ggproto.md)
  [`StatRowsDensity2dFilled`](ordr-ggproto.md)
  [`StatColsDensity2dFilled`](ordr-ggproto.md)
  [`StatRowsEllipse`](ordr-ggproto.md)
  [`StatColsEllipse`](ordr-ggproto.md)
  [`StatRowsCenter`](ordr-ggproto.md)
  [`StatColsCenter`](ordr-ggproto.md) [`StatRowsStar`](ordr-ggproto.md)
  [`StatColsStar`](ordr-ggproto.md) [`StatRowsChull`](ordr-ggproto.md)
  [`StatColsChull`](ordr-ggproto.md) [`StatRowsPeel`](ordr-ggproto.md)
  [`StatColsPeel`](ordr-ggproto.md) [`StatRowsCone`](ordr-ggproto.md)
  [`StatColsCone`](ordr-ggproto.md) [`StatRowsDepth`](ordr-ggproto.md)
  [`StatColsDepth`](ordr-ggproto.md)
  [`StatRowsDepthFilled`](ordr-ggproto.md)
  [`StatColsDepthFilled`](ordr-ggproto.md)
  [`StatRowsScale`](ordr-ggproto.md) [`StatColsScale`](ordr-ggproto.md)
  [`StatRowsSpantree`](ordr-ggproto.md)
  [`StatColsSpantree`](ordr-ggproto.md)
  [`StatRowsBagplot`](ordr-ggproto.md)
  [`StatColsBagplot`](ordr-ggproto.md) [`StatRowsRule`](ordr-ggproto.md)
  [`StatColsRule`](ordr-ggproto.md)
  [`StatRowsProjection`](ordr-ggproto.md)
  [`StatColsProjection`](ordr-ggproto.md) : ggproto classes created and
  adapted for ordr
- [`geom_rows_point()`](biplot-geoms.md)
  [`geom_cols_point()`](biplot-geoms.md)
  [`geom_rows_path()`](biplot-geoms.md)
  [`geom_cols_path()`](biplot-geoms.md)
  [`geom_rows_polygon()`](biplot-geoms.md)
  [`geom_cols_polygon()`](biplot-geoms.md)
  [`geom_rows_contour()`](biplot-geoms.md)
  [`geom_cols_contour()`](biplot-geoms.md)
  [`geom_rows_density_2d()`](biplot-geoms.md)
  [`geom_cols_density_2d()`](biplot-geoms.md)
  [`geom_rows_density_2d_filled()`](biplot-geoms.md)
  [`geom_cols_density_2d_filled()`](biplot-geoms.md)
  [`geom_rows_text()`](biplot-geoms.md)
  [`geom_cols_text()`](biplot-geoms.md)
  [`geom_rows_label()`](biplot-geoms.md)
  [`geom_cols_label()`](biplot-geoms.md)
  [`geom_rows_text_repel()`](biplot-geoms.md)
  [`geom_cols_text_repel()`](biplot-geoms.md)
  [`geom_rows_label_repel()`](biplot-geoms.md)
  [`geom_cols_label_repel()`](biplot-geoms.md)
  [`geom_rows_axis()`](biplot-geoms.md)
  [`geom_cols_axis()`](biplot-geoms.md)
  [`geom_rows_pointranges()`](biplot-geoms.md)
  [`geom_cols_pointranges()`](biplot-geoms.md)
  [`geom_rows_lineranges()`](biplot-geoms.md)
  [`geom_cols_lineranges()`](biplot-geoms.md)
  [`geom_rows_isoline()`](biplot-geoms.md)
  [`geom_cols_isoline()`](biplot-geoms.md)
  [`geom_rows_text_radiate()`](biplot-geoms.md)
  [`geom_cols_text_radiate()`](biplot-geoms.md)
  [`geom_rows_vector()`](biplot-geoms.md)
  [`geom_cols_vector()`](biplot-geoms.md)
  [`geom_rows_bagplot()`](biplot-geoms.md)
  [`geom_cols_bagplot()`](biplot-geoms.md)
  [`geom_rows_rule()`](biplot-geoms.md)
  [`geom_cols_rule()`](biplot-geoms.md)
  [`geom_rows_interpolation()`](biplot-geoms.md)
  [`geom_cols_interpolation()`](biplot-geoms.md) : Convenience geoms for
  row and column matrix factors
- [`stat_rows_density_2d()`](biplot-stats.md)
  [`stat_cols_density_2d()`](biplot-stats.md)
  [`stat_rows_density_2d_filled()`](biplot-stats.md)
  [`stat_cols_density_2d_filled()`](biplot-stats.md)
  [`stat_rows_ellipse()`](biplot-stats.md)
  [`stat_cols_ellipse()`](biplot-stats.md)
  [`stat_rows_center()`](biplot-stats.md)
  [`stat_cols_center()`](biplot-stats.md)
  [`stat_rows_star()`](biplot-stats.md)
  [`stat_cols_star()`](biplot-stats.md)
  [`stat_rows_chull()`](biplot-stats.md)
  [`stat_cols_chull()`](biplot-stats.md)
  [`stat_rows_peel()`](biplot-stats.md)
  [`stat_cols_peel()`](biplot-stats.md)
  [`stat_rows_cone()`](biplot-stats.md)
  [`stat_cols_cone()`](biplot-stats.md)
  [`stat_rows_depth()`](biplot-stats.md)
  [`stat_cols_depth()`](biplot-stats.md)
  [`stat_rows_depth_filled()`](biplot-stats.md)
  [`stat_cols_depth_filled()`](biplot-stats.md)
  [`stat_rows_scale()`](biplot-stats.md)
  [`stat_cols_scale()`](biplot-stats.md)
  [`stat_rows_spantree()`](biplot-stats.md)
  [`stat_cols_spantree()`](biplot-stats.md)
  [`stat_rows_bagplot()`](biplot-stats.md)
  [`stat_cols_bagplot()`](biplot-stats.md)
  [`stat_rows_rule()`](biplot-stats.md)
  [`stat_cols_rule()`](biplot-stats.md)
  [`stat_rows_projection()`](biplot-stats.md)
  [`stat_cols_projection()`](biplot-stats.md) : Convenience stats for
  row and column matrix factors

## Class methods

Recovery and augmentation methods for ordination model classes

- [`as_tbl_ord(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_rows(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_cols(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_inertia(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_coord(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_conference(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_supp_rows(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_supp_cols(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_aug_rows(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_aug_cols(`*`<cancor_ord>`*`)`](methods-cancor.md)
  [`recover_aug_coord(`*`<cancor_ord>`*`)`](methods-cancor.md) :
  Functionality for canonical correlations
- [`as_tbl_ord(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_rows(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_cols(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_inertia(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_coord(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_conference(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_aug_rows(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_aug_cols(`*`<cmds_ord>`*`)`](methods-cmds.md)
  [`recover_aug_coord(`*`<cmds_ord>`*`)`](methods-cmds.md) :
  Functionality for classical multidimensional scaling objects
- [`as_tbl_ord(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_rows(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_cols(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_inertia(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_conference(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_coord(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_aug_rows(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_aug_cols(`*`<correspondence>`*`)`](methods-correspondence.md)
  [`recover_aug_coord(`*`<correspondence>`*`)`](methods-correspondence.md)
  : Functionality for correspondence analysis ('correspondence') objects
- [`as_tbl_ord(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_rows(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_cols(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_inertia(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_coord(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_conference(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_aug_rows(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_aug_cols(`*`<eigen>`*`)`](methods-eigen.md)
  [`recover_aug_coord(`*`<eigen>`*`)`](methods-eigen.md)
  [`as_tbl_ord(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_rows(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_cols(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_inertia(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_coord(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_conference(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_aug_rows(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_aug_cols(`*`<eigen_ord>`*`)`](methods-eigen.md)
  [`recover_aug_coord(`*`<eigen_ord>`*`)`](methods-eigen.md) :
  Functionality for eigen-decompositions
- [`as_tbl_ord(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_rows(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_cols(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_inertia(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_coord(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_conference(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_supp_rows(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_aug_rows(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_aug_cols(`*`<factanal>`*`)`](methods-factanal.md)
  [`recover_aug_coord(`*`<factanal>`*`)`](methods-factanal.md) :
  Functionality for factor analysis ('factanal') objects
- [`as_tbl_ord(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_rows(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_cols(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_coord(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_aug_rows(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_aug_cols(`*`<kmeans>`*`)`](methods-kmeans.md)
  [`recover_aug_coord(`*`<kmeans>`*`)`](methods-kmeans.md) :
  Functionality for k-means clustering ('kmeans') objects
- [`as_tbl_ord(`*`<lda>`*`)`](methods-lda.md)
  [`as_tbl_ord(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_rows(`*`<lda>`*`)`](methods-lda.md)
  [`recover_rows(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_cols(`*`<lda>`*`)`](methods-lda.md)
  [`recover_cols(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_inertia(`*`<lda>`*`)`](methods-lda.md)
  [`recover_inertia(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_coord(`*`<lda>`*`)`](methods-lda.md)
  [`recover_coord(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_conference(`*`<lda>`*`)`](methods-lda.md)
  [`recover_conference(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_aug_rows(`*`<lda>`*`)`](methods-lda.md)
  [`recover_aug_rows(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_aug_cols(`*`<lda>`*`)`](methods-lda.md)
  [`recover_aug_cols(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_aug_coord(`*`<lda>`*`)`](methods-lda.md)
  [`recover_aug_coord(`*`<lda_ord>`*`)`](methods-lda.md)
  [`recover_supp_rows(`*`<lda>`*`)`](methods-lda.md)
  [`recover_supp_rows(`*`<lda_ord>`*`)`](methods-lda.md) : Functionality
  for linear discriminant analysis ('lda') objects
- [`as_tbl_ord(`*`<lm>`*`)`](methods-lm.md)
  [`recover_rows(`*`<lm>`*`)`](methods-lm.md)
  [`recover_cols(`*`<lm>`*`)`](methods-lm.md)
  [`recover_coord(`*`<lm>`*`)`](methods-lm.md)
  [`recover_aug_rows(`*`<lm>`*`)`](methods-lm.md)
  [`recover_aug_cols(`*`<lm>`*`)`](methods-lm.md)
  [`recover_aug_coord(`*`<lm>`*`)`](methods-lm.md)
  [`recover_aug_rows(`*`<glm>`*`)`](methods-lm.md)
  [`recover_rows(`*`<mlm>`*`)`](methods-lm.md)
  [`recover_cols(`*`<mlm>`*`)`](methods-lm.md)
  [`recover_coord(`*`<mlm>`*`)`](methods-lm.md)
  [`recover_aug_rows(`*`<mlm>`*`)`](methods-lm.md)
  [`recover_aug_cols(`*`<mlm>`*`)`](methods-lm.md)
  [`recover_aug_coord(`*`<mlm>`*`)`](methods-lm.md) : Functionality for
  linear model objects
- [`as_tbl_ord(`*`<lra>`*`)`](methods-lra.md)
  [`recover_rows(`*`<lra>`*`)`](methods-lra.md)
  [`recover_cols(`*`<lra>`*`)`](methods-lra.md)
  [`recover_inertia(`*`<lra>`*`)`](methods-lra.md)
  [`recover_coord(`*`<lra>`*`)`](methods-lra.md)
  [`recover_conference(`*`<lra>`*`)`](methods-lra.md)
  [`recover_aug_rows(`*`<lra>`*`)`](methods-lra.md)
  [`recover_aug_cols(`*`<lra>`*`)`](methods-lra.md)
  [`recover_aug_coord(`*`<lra>`*`)`](methods-lra.md) : Functionality for
  log-ratio analysis ('lra') objects
- [`as_tbl_ord(`*`<mca>`*`)`](methods-mca.md)
  [`recover_rows(`*`<mca>`*`)`](methods-mca.md)
  [`recover_cols(`*`<mca>`*`)`](methods-mca.md)
  [`recover_inertia(`*`<mca>`*`)`](methods-mca.md)
  [`recover_conference(`*`<mca>`*`)`](methods-mca.md)
  [`recover_coord(`*`<mca>`*`)`](methods-mca.md)
  [`recover_supp_rows(`*`<mca>`*`)`](methods-mca.md)
  [`recover_aug_rows(`*`<mca>`*`)`](methods-mca.md)
  [`recover_aug_cols(`*`<mca>`*`)`](methods-mca.md)
  [`recover_aug_coord(`*`<mca>`*`)`](methods-mca.md) : Functionality for
  multiple correspondence analysis ('mca') objects
- [`as_tbl_ord(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_rows(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_cols(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_inertia(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_coord(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_conference(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_aug_rows(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_aug_cols(`*`<prcomp>`*`)`](methods-prcomp.md)
  [`recover_aug_coord(`*`<prcomp>`*`)`](methods-prcomp.md) :
  Functionality for principal components analysis ('prcomp') objects
- [`as_tbl_ord(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_rows(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_cols(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_inertia(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_coord(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_conference(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_supp_rows(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_aug_rows(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_aug_cols(`*`<princomp>`*`)`](methods-princomp.md)
  [`recover_aug_coord(`*`<princomp>`*`)`](methods-princomp.md) :
  Functionality for principal components analysis ('princomp') objects
- [`as_tbl_ord(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_rows(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_cols(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_inertia(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_coord(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_conference(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_aug_rows(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_aug_cols(`*`<svd_ord>`*`)`](methods-svd.md)
  [`recover_aug_coord(`*`<svd_ord>`*`)`](methods-svd.md) : Functionality
  for singular value decompositions
