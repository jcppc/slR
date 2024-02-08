#' Title
#'
#' @param filename the filename for the SLR data
#'
#' @return
#' @export
#'
#' @examples
read <- function( filename ) {

  #slr_new <-  methods::setRefClass("SLR", slots = list(name="character", age="numeric", role="character") )
  #new("SLR", name = "Peter", age = 21, role = "Developer")

  slr <-  readxl::read_excel( filename )
  return(slr)
}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param authors The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#'
#' @return
#' @export
#'
#' @examples
generate_slr_components <- function( slr, authors, year.above = 1900, output = ".", save.pdf = TRUE)
{

  # Latex functions
  slR::write_comments( slr, output = output.folder )
  slR::write_authors( authors, output = output.folder )
  slR::write_institutions( authors, output = output.folder )
  slR::write_countries( authors, output = output.folder )
  slR::write_continents( authors, output = output.folder )
  slR::write_articles( slr, output = output.folder )
  slR::write_graphics( output = output.folder )


  # Plot functions
  slR::score_per_year_boxplot( slr, year.above, output, save.pdf )
  slR::score_per_year_barchart( slr, year.above, output, save.pdf )
  slR::score_per_venue_barchart( slr, year.above, output, save.pdf )
  slR::score_per_author_barchart( slr, year.above, output, save.pdf )
  slR::score_per_publication_barchart( slr, year.above, output, save.pdf )

}




