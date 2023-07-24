
# tableclothr

<!-- badges: start -->
<!-- badges: end -->

tableclothr is an R package that offers a set of flexible wrappers and supporting
functions for the creation and manipulation of gt and DT tables. Designed
with  simplicity in mind, users can seamlessly generate themed tables
quickly without the need for lengthy and redundant coding.

## Installation

You can install the development version of tableclothr from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("hrkring/tableclothr")
```

## Usage

The primary function in tableclothr is `tablecloth()`:

``` r
library(tableclothr)

## Create a static table with gt as the base
tablecloth(gtcars, format = "static", theme = "greyscale")

## Create a dynamic table with DT as the base
tablecloth(gtcars, format = "dynamic", theme = "greyscale")
```

