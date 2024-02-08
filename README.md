
<!-- README.md is generated from README.Rmd. Please edit that file -->

# slR

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![Launch
binder](https://mybinder.org/badge_logo.svg)](https://mybinder.org/v2/gh/jcppc/slR/main)
<!-- badges: end -->

The goal of **slR** package is to provide researchers with a tool to
quickly build Systematic Literature Reviews outputs to include into
their documents. These outputs include: data summarizarion and charts,
latex tables and more….

## Installation

You can install the development version of **slR** from
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

# Folder to save the generated files
output.folder <- "/users/jcppc/output"

# Step 1
# If you have only a bib file with the articles, load this file
slr.file <- "/users/jcppc/slr-articles.bib"
slr <-  slR::read( slr.file, output = output.folder )

# Step 2
# Otherwise, if you have the Excel files already created in Step 1, load them
# Once you do Step 1, you dont' need to run it anymore, because you can always start from Step 2
slr.file <- "/users/jcppc/slr-articles.xlsx"
authors.file <- "/users/jcppc/slr-authors.xlsx"

slr = list()
slr$articles <- slR::read( slr.file, output = output.folder )
slr$authors <- slR::read( authors.file, output = output.folder )


# Print the package version

slR::version()

# Latex related functions

slR::write_comments( slr$articles, output = output.folder )
slR::write_authors( slr$authors, output = output.folder )
slR::write_institutions( slr$authors, output = output.folder )
slR::write_countries( slr$authors, output = output.folder )
slR::write_continents( slr$authors, output = output.folder )
slR::write_articles( slr$articles, output = output.folder )
slR::write_graphics( output = output.folder )
 
# Plots related functions

slR::score_per_year_boxplot( slr$articles, output = output.folder )
slR::score_per_year_barchart( slr$articles, output = output.folder )
slR::score_per_venue_barchart( slr$articles, output = output.folder )
slR::score_per_author_barchart( slr$articles, output = output.folder )
slR::score_per_publication_barchart( slr$articles, output = output.folder )

# Generate all components

slR::generate_slr_components( slr, output = output.folder )


# Add new dimensions to the slr object to reflect the RQs to answer
# This needs to be done always manually because it depends on your Research Questions

dimensions <- c("Dim1","Dim2","Dim3","Dim4")
slR::add_dimensions( slr, dimensions, output = output.folder )
```
