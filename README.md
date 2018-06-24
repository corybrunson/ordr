# **tidybiplot**: A unified framework for handling, manipulating, and visualizing ordination data

## motivation

This package is designed to integrate ordination analysis and biplot visualization into a [**tidyverse**](https://github.com/tidyverse/tidyverse) workflow.

## usage

**tidybiplot** remains under development and will be incremented to v1.0.0 for the first CRAN release. Once it has full R package infrastructure, it will be installable using [**devtools**](https://github.com/r-lib/devtools):

```r
devtools::install_bitbucket("corybrunson/tidybiplot")
```

## contribute

This early version only handles a few types of biplot data, a few transformations, and a few **ggplot2** layers. My hope is that prospective contributors can see how to expand (and improve!) on the implementations thus far.

Contributing guidelines will be posted soon. In the meantime, please respect the [**tidyverse** contributing guidelines](https://www.tidyverse.org/articles/2017/08/contributing/) and be supportive!

## acknowledgments

This package was originally inspired by **ggbiplot** by [vqv](https://github.com/vqv/ggbiplot), [richardjtelford](https://github.com/richardjtelford/ggbiplot), and [GegnzaV](https://github.com/GegznaV/ggbiplot), among others. So far as i know, it first brought biplots into the **tidyverse** framework. The motivation to unify a variety of ordination methods came from several books and articles [by Michael Greenacre](https://www.barcelonagse.eu/research/publications/all?author=Michael%20Greenacre), and [thomasp85's **tidygraph**](https://github.com/thomasp85/tidygraph) sequel to **ggraph** finally induced the shift from generating scatterplots to handling and manipulating ordination data.
