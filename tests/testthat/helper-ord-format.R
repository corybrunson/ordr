# Shared fixtures for format/print tests

# Standard PCA: 150 rows, 4 coords, 0 annotations
ord_pca <- ordinate(iris[1:4], prcomp)

# LDA with supplementary elements
ord_lda <- ordinate(iris[1:4], lda_ord, grouping = iris$Species)

# Small dataset: 3 rows, 3 coords
ord_small <- as_tbl_ord(
  make_tbl_ord(
    rows = matrix(1:9, 3, 3, dimnames = list(NULL, c("A", "B", "C"))),
    cols = matrix(1:9, 3, 3, dimnames = list(NULL, c("A", "B", "C")))
  )
)

# strip ANSI escape sequences for portable snapshots
strip_style <- function(x) {
  gsub("\033\\[[0-9;]*m", "", x)
}

# build footer from get_ord_layout args
footer_of <- function(...) {
  layout <- get_ord_layout(...)
  alloc <- ord_width_alloc(layout)
  fmt_ann <- ord_format_ann(layout, alloc)
  ord_footer(layout, fmt_ann)
}

# rejoin pillar's wrapped note lines and normalize ellipsis glyphs
# (pillar renders ASCII variants in non-unicode environments)
unwrap_note <- function(x) {
  x <- gsub("\n#\\s*", " ", x)
  gsub("\u2026", "...", x, fixed = TRUE)
}
