
<!-- README.md is generated from README.Rmd. Please edit that file -->
endpointR <img src='man/figures/logo.png' align="right" height="139" />
=======================================================================

endpointR
=========

<!-- badges: start -->
<!-- badges: end -->
The goal of endpointR is to ...

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mytalbot/endpointR")
library(endpointR)
```

Example
-------

This is a basic example uses the internal Glioma data from Helgers & Talbot et al 2019 to indicate potential endpoints for individual animals. The input variables are raw body weight data.

``` r
library(endpointR)
a      <- 1
td     <- ep_select(gliodat, a) 
danger <- epR(td        = td,
              org       = FALSE,
              wl        = 6,
              SDwdth    = 2,
              mad       = FALSE,
              blind     = TRUE)
danger
#>    n timepoint value where
#> 1 15        12 99.25 lower
#> 2 15        15 94.14 lower
```
