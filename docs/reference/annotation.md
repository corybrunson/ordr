# Annotate factors of 'tbl_ord' objects

These functions annotate the matrix factors of [tbl_ord](tbl_ord.md)s
with additional variables, and retrieve these annotations.

The unexported `annotation_*()` and `set_annotation_*()` functions
assign and retrieve values of the `"*_annotation"` attributes of `x`,
which must have the same number of rows as `get_*(x)`.

## Arguments

- annot:

  A [data.frame](https://rdrr.io/r/base/data.frame.html) having the same
  number of rows as `get_*(x)`.

## See also

[augmentation](augmentation.md) methods that must interface with
annotation.
