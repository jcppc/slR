
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

slR::write_comments( slr, output = output.folder )
slR::write_authors( slr, output = output.folder )
slR::write_institutions( slr, output = output.folder )
slR::write_countries( slr, output = output.folder )
slR::write_continents( slr, output = output.folder )
slR::write_articles( slr, output = output.folder )
slR::write_sample_table_template( output = output.folder )
slR::write_graphics( output = output.folder )


# Plots related functions

slR::score_per_year_boxplot( slr, output = output.folder )
slR::score_per_year_barchart( slr, output = output.folder )
slR::score_per_venue_barchart( slr, output = output.folder )
slR::score_per_author_barchart( slr, output = output.folder )
slR::score_per_publication_barchart( slr, output = output.folder )
slR::author_per_year( slr, output = output.folder )
slR::publication_per_year( slr, output = output.folder )
slR::venue_per_year( slr, output = output.folder )

# Add new dimensions to the slr object to reflect the RQs to answer
# This needs to be done always manually because it depends on your Research Questions

dimensions <- c("Dim1","Dim2","Dim3","Dim4")
slR::add_dimensions( slr, dimensions, output = output.folder )

# Now go back to you Excel file and categorize/fill in your dimensions

# Plots for each dimension
# Needs to be done for each dimension manually

slR::plot_dimension_frequency( slr, output = output.folder, plot.name = "plot-freq-Dim1.pdf", dimension = "Dim1" )
slR::plot_dimension_per_year( slr, output = output.folder, plot.name = "plot-Dim1.pdf", dimension = "Dim1" )
slR::plot_dimension_frequency( slr, output = output.folder, plot.name = "plot-freq-Dim2.pdf", dimension = "Dim2" )
slR::plot_dimension_per_year( slr, output = output.folder, plot.name = "plot-Dim2.pdf", dimension = "Dim2" )

# Latex tables with frequencies per dimension
# Needs to be done for each dimension

slR::write_frequency_of_dimension( slr, output = output.folder, dimension = "Dim1" )
slR::write_frequency_of_dimension( slr, output = output.folder, dimension = "Dim2" )


# Generate all default components ( except the ones dependent on the dimensions created )

slR::generate_slr_dimensions_latex( slr, output = output.folder, dimensions = dimensions )


# Generate all default components ( except the ones dependent on the dimensions created )

slR::generate_slr_components( slr, output = output.folder )

# Generate all components, including the ones related with the dimensions

dimensions <- c("Dim1","Dim2","Dim3","Dim4")
slR::generate_slr_components( slr, output = output.folder, dimensions = dimensions )
```
