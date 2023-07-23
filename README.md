
# tableclothr

<!-- badges: start -->
<!-- badges: end -->

tableclothr is a stream-lined and user-friendly R package that offers a set of
flexible wrappers for effortless creation of static and dynamic tables. Designed
with  simplicity in mind, users can seamlessly generate themed gt and DT tables
quickly without the need for lengthy and redundant coding.

## Installation

You can install the development version of tableclothr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hrkring/tableclothr")
```

## Example

This is the primary function in tableclothr:

``` r
library(tableclothr)

## Create a static table with gt as the base
tablecloth(gtcars, format = "static", theme = "greyscale")

## Create a dynamic table with DT as the base
tablecloth(gtcars, format = "dynamic", theme = "greyscale")
```

