
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jGBV

<!-- badges: start -->

[![R-CMD-check](https://github.com/BroVic/jGBV/workflows/R-CMD-check/badge.svg)](https://github.com/BroVic/jGBV/actions)
<!-- badges: end -->

Convenience R package for the Jhpiego Nigeria’s Gender-based Violence
(GBV) Assessments

## About this package

The data analysis for the various assessments is quite extensive and
multi-faceted. Also, great care is taken to ensure that the projects are
reproducible and extensible.

This package contains R functions and objects that were created to help
us attain those goals. The actual material i.e. data, documents, etc,
are domiciled in their various RStudio projects and are not publicly
accessible.

## Installation

The package can be easily downloaded and installed into R from GitHub
using the code below. Note that the `INSTALL_opts` argument is mandatory
to enable successful installs on machines that use both 32- and 64-bit
architectures.

``` r
# install.packages("remotes")
remotes::install_github("BroVic/jGBV", INSTALL_opts = c("--no-multiarch"))
```
