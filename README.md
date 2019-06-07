
<!-- README.md is generated from README.Rmd. Please edit that file -->
endpointR <img src="https://talbotsr.com/endpointR/logo.png" align="right" height="139" />
==========================================================================================

endpointR
=========

<!-- badges: start -->
[![Travis build status](https://travis-ci.org/mytalbot/endpointR.svg?branch=master)](https://travis-ci.org/r-lib/usethis) <!-- badges: end -->

The endpointR is a computational tool for aiding scientists in laboratory animal research to identify humane endpoints. Originally it was developed for using body weight (change) data but it can be used with any time series data. However, it is not meant as substitution for experience and reason - but it may help in any decision making process concerning the wellbeing of an animal.

Installation
------------

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("mytalbot/endpointR")
library(endpointR)
```

endpointR Shiny App
-------------------

The endpointR is ready-to-use as a Shiny app for body weight data analysis under the following link.

[endpointR](https://calliope.shinyapps.io/endpointer/)

Please restrict your online time on the app and close the browser when finished as we're only using a public license. Thank you!

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
