# A synchronization flag.

The inert function `sync()` operates analogously to
[`ggplot2::waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
to indicate that an auxiliary aesthetic should be synchronized to a
standard aesthetic (when it is mapped from data). `is.sync()` reports
whether an object is of this class.

## Usage

``` r
sync()

is.sync(x)
```

## Arguments

- x:

  An object to test
