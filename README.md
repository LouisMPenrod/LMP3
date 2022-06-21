
<!-- README.md is generated from README.Rmd. Please edit that file -->

# LMP3

<!-- badges: start -->
<!-- badges: end -->

## Overview

The goal of LMP3 is to contain functions developed for my colleagues or
myself. The package currently contains:

-   `factor_sort()` allows the user to utilize a shiny app to more
    easily rearrange the levels of a factor.
-   `rcorr_fliter()` provides a method to reduce large rcorr correlation
    matrices into simpler data frames and filter to match specific
    conditions.
-   `lumpmean()` provides a method to use a running mean that averages
    the observations with a window of a specific time, not the number of
    observations.
-   `count2d()` counts the number of observations in a two-dimensional
    space.
-   `reextent()` changes the extend of a raster from -180:180 to 0:360
    and vice versa.
-   `ineq()` counts the number of values that meet an equality or
    inequality condition.

## Installation

You can install the development version of LMP3 from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("LouisMPenrod/LMP3")
```

## Getting help

If you encounter an issue, please file an issue or contact the package
author with a minimal reproducible example.
