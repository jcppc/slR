
explode.authors <- function( slr, output ) {

  authors <- slr %>% tidyr::separate_rows(Authors, sep = "#")

  authors <-  data.frame( Study = authors$Study,
                      Citation = authors$Citation,
                      Year = authors$Year,
                      Venue = authors$Venue,
                      Title = authors$Title,
                      Publication = authors$Publication,
                      Author = authors$Authors,
                      Department = "Fill this field (change this)",
                      Institution = "Fill this field (change this)",
                      City = "Fill this field (change this)",
                      Country = "Fill this field (change this)",
                      Continent = "Fill this field (change this)" )
  openxlsx::write.xlsx( authors, file = paste0(output, "/", "slr-authors.xlsx"), colNames = TRUE)
  return( authors )
}


read.bib <- function( filename, output ) {

  #path <- system.file( filename, package = "bib2df" )
  #sapply(bib$AUTHOR, paste, collapse = ", ")
  bib <- as.data.frame( bib2df::bib2df( filename ) )
  slr <-  data.frame( Study = sprintf("S%02d", 1:nrow(bib) ),
                      Citation = bib$BIBTEXKEY,
                      Year = as.numeric(bib$YEAR),
                      Venue = bib$CATEGORY,
                      Title = bib$TITLE,
                      Comments = "Put your comments about this article here...",
                      Publication = bib$JOURNAL,
                      Author = "Author et al. (change this)",
                      Authors = sapply(bib$AUTHOR, paste, collapse = "#"),
                      DOI = bib$DOI,
                      Total = round(stats::rnorm(nrow(bib), mean = 10, sd = 2),0) )
  slr$Venue <-  plyr::revalue(slr$Venue, c("INBOOK"="Book Section","INCOLLECTION"="Book Section","ARTICLE"="Journal", "INPROCEEDINGS"="Conference", "BOOK"="Book", "WORKSHOP"="Workshop", "TECHREPORT"="Report"), warn_missing = F )
  slr[is.na(slr)] <- 0
  openxlsx::write.xlsx( slr, file = paste0(output, "/", "slr-articles.xlsx"), colNames = TRUE)
  authors <- explode.authors( slr, output )
  result = list()
  result$articles <- slr
  result$authors <-  authors

  return( result )
}


#' Read xlsx and/or bib files containing the articles for the SLR
#'
#' @param filename the filename for the SLR data
#' @param output Folder where to write the Excel file with the articles (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
read <- function( filename, output = ".") {
  suppressWarnings({
  extension <- tools::file_ext( filename )
  if (extension == "bib") { return( read.bib( filename, output ) )  }
  else if (extension == "xlsx") { return( readxl::read_excel( filename ) ) }
  else {  message("\nInvalid file type. Use xlsx or bib files.\n") ; return( NULL )}
  })
}

#' Title
#'
#' @param slr SLR data
#' @param dimensions New dimensions(columns to add to the Excel file)
#' @param output Folder where to write the Excel file with the articles (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
add_dimensions <- function( slr, dimensions, output = ".")
{
  slr$articles[ , dimensions] <- "Add values here. Split them by a comma"
  openxlsx::write.xlsx( slr$articles, file = paste0(output, "/", "slr-articles.xlsx"), colNames = TRUE)
  return(slr)
}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param dimensions List of dimensions to generate latex and plots
#'
#' @return
#' @export
#'
#' @examples
generate_slr_components <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE, dimensions )
{

  suppressWarnings({

  extrafont::loadfonts(device = "all", quiet = TRUE)

  # Latex functions

  slR::write_comments( slr, output = output.folder )
  slR::write_authors( slr, output = output.folder )
  slR::write_institutions( slr, output = output.folder )
  slR::write_countries( slr, output = output.folder )
  slR::write_continents( slr, output = output.folder )
  slR::write_articles( slr, output = output.folder )
  #slR::write_sample_table_template( output = output.folder )
  slR::write_graphics( output = output.folder )
  if (!missing(dimensions)) slR::generate_slr_dimensions_latex( slr, output = output.folder, dimensions = dimensions )

  # Plot functions

  slR::score_per_year_boxplot( slr, year.above = year.above, output = output.folder )
  slR::score_per_year_barchart( slr, year.above = year.above, output = output.folder )
  slR::score_per_venue_barchart( slr, year.above = year.above, output = output.folder )
  slR::score_per_author_barchart( slr, year.above = year.above, output = output.folder )
  slR::score_per_publication_barchart( slr, year.above = year.above, output = output.folder )
  slR::author_per_year( slr, year.above = year.above, output = output.folder )
  slR::publication_per_year( slr, year.above = year.above, output = output.folder )
  slR::venue_per_year( slr, year.above = year.above, output = output.folder )
  if (!missing(dimensions)) slR::generate_slr_dimensions_plot( slr, output = output.folder, dimensions = dimensions )

  })

}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param dimensions List of dimensions to generate latex and plots
#'
#' @return
#' @export
#'
#' @examples
generate_slr_dimensions_latex <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE, dimensions )
{
  dims <- lapply( dimensions, write_frequency_of_dimension, slr = slr, output = output)

}



#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param dimensions List of dimensions to generate latex and plots
#'
#' @return
#' @export
#'
#' @examples
generate_slr_dimensions_plot <- function( slr, year.above = 1900, output = ".", save.pdf = TRUE, dimensions )
{
  year <- lapply( dimensions, plot_dimension_per_year, slr = slr, output = output)
  frequency <-  lapply( dimensions, plot_dimension_frequency, slr = slr, output = output)

  #slR::plot_dimension_per_year( slr, output = output.folder, plot.name = "plot-Dim1.pdf", dimension = "Dim1" )
  #slR::plot_dimension_frequency( slr, output = output.folder, plot.name = "plot-freq-Dim2.pdf", dimension = "Dim2" )


}



