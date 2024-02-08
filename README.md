
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
devtools::install_github("jcppc/slR")
```

## Setup

To properly render the charts, the **extrafont** package should be
installed and fonts imported:

``` r
install.packages("extrafont")
extrafont::font_import()
extrafont::loadfonts(device = "all", quiet = TRUE)
```

## Example

This is a basic example which shows you how to use the library:

``` r
# Basic example code

library(slR)

slr.file <- "/users/jcppc/slr-articles.xlsx"
authors.file <- "/users/jcppc/articles-authors.xlsx"
output.folder <- "/users/jcppc/output"

slr <-  slR::read( slr.file )
authors <-  slR::read( authors.file )

# Print the package version

slR::version()

# Latex related functions

slR::write_comments( slr, output = output.folder )
slR::write_authors( authors, output = output.folder )
slR::write_institutions( authors, output = output.folder )
slR::write_countries( authors, output = output.folder )
slR::write_continents( authors, output = output.folder )
slR::write_articles( slr, output = output.folder )
slR::write_graphics( output = output.folder )
 
# Plots related functions

slR::score_per_year_boxplot( slr, output = output.folder )
slR::score_per_year_barchart( slr, output = output.folder )
slR::score_per_venue_barchart( slr, output = output.folder )
slR::score_per_author_barchart( slr, output = output.folder )
slR::score_per_publication_barchart( slr, output = output.folder )

# Generate all components

slR::generate_slr_components( slr, authors, output = output.folder )
  
```
