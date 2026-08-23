# Convenience coordinate system for scaffolding axes

2- (and 3-) dimensional biplots require that coordinates lie on the same
scale but may additionally benefit from a square plotting window. While
`CoordRect` provides control of coordinate and window aspect ratios, the
convenience `CoordScaffold` system also fixes the coordinate aspect
ratio at `1` and gives the user control only of the plotting window.

## Usage

``` r
coord_scaffold(
  window_ratio = 1,
  xlim = NULL,
  ylim = NULL,
  expand = TRUE,
  clip = "on"
)
```

## Arguments

- window_ratio:

  aspect ratio of plotting window

## Examples

``` r
# resize the plot to see that the specified aspect ratio is maintained
p <- ggplot(mtcars, aes(mpg, hp/10)) + geom_point()
p + coord_scaffold()

p + coord_scaffold(window_ratio = 2)


# prevent rescaling in response to `theme()` aspect ratio
p <- ggplot(mtcars, aes(mpg, hp/5)) + geom_point()
p + coord_equal() + theme(aspect.ratio = 1)

p + coord_scaffold() + theme(aspect.ratio = 1)


# NB: `theme(aspect.ratio = )` overrides `Coord*$aspect`:
p + coord_fixed(ratio = 1) + theme(aspect.ratio = 1)

p + coord_scaffold(window_ratio = 2) + theme(aspect.ratio = 1)
```
