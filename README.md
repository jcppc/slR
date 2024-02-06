
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jcppc/slR/main)
<!-- badges: end -->

The goal of slR package is to provide researchers with a tool to quickly
build Systematic Literature Reviews outputs to include into their
documents. These outputs include: data summarizarion and charts, latex
tables and more….

## Installation

You can install the development version of slR from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
install.packages("extrafont")

devtools::install_github("jcppc/slR")
```

## Setup

To properly render the charts, the **extrafont** package should be
installed and then loaded

``` r
library(extrafont)
extrafont::font_import()
```

## Example

This is a basic example which shows you how to use the library:

``` r
# Basic example code
library(slR)

slr.file <- "/users/jcppc/slr-articles.xlsx"
authors.file <- "/users/jcppc/articles-authors.xlsx"
output.folder <- "/users/jcppc/output"

slr <-  slR::build_slr( slr.file )
authors <-  slR::build_slr( authors.file )

slR::writeComments( slr, output = output.folder )
slR::writeSummaryTable( authors, output = output.folder )
slR::boxplot_per_year( slr, year.above = 2012, output = output.folder, save.pdf = FALSE )
```
