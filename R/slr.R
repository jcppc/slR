
explode.authors <- function( slr, output ) {

  authors <- slr %>% tidyr::separate_rows(Author, sep = "#")

  authors <-  data.frame( Study = authors$Study,
                      Citation = authors$Citation,
                      Year = authors$Year,
                      Venue = authors$Venue,
                      Title = authors$Title,
                      Publication = authors$Publication,
                      Author = authors$Author,
                      Department = "Fill this field",
                      Institution = "Fill this field",
                      City = "Fill this field",
                      Country = "Fill this field",
                      Continent = "Fill this field" )
  openxlsx::write.xlsx( authors, file = paste0(output, "/", "articles-authors.xlsx"), colNames = TRUE)
  return( authors )
}

read.bib <- function( filename, output ) {

  #path <- system.file( filename, package = "bib2df" )
  #sapply(bib$AUTHOR, paste, collapse = ", ")
  bib <- as.data.frame( bib2df::bib2df( filename ) )
  #print(nrow(bib))
  slr <-  data.frame( Study = sprintf("S%02d", 1:nrow(bib) ),
                      Citation = bib$BIBTEXKEY,
                      Year = as.numeric(bib$YEAR),
                      Venue = bib$CATEGORY,
                      Title = bib$TITLE,
                      Comments = "",
                      Publication = bib$JOURNAL,
                      Author = sapply(bib$AUTHOR, paste, collapse = "#"),
                      DOI = bib$DOI,
                      Total = round(stats::rnorm(nrow(bib), mean = 10, sd = 2),0) )
  slr$Venue <-  plyr::revalue(slr$Venue, c("ARTICLE"="Journal", "INPROCEEDINGS"="Conference", "BOOK"="Book", "WORKSHOP"="Workshop", "TECHREPORT"="Report"), warn_missing = F )
  slr[is.na(slr)] <- 0
  openxlsx::write.xlsx( slr, file = paste0(output, "/", "slr-articles.xlsx"), colNames = TRUE)
  authors <- explode.authors( slr, output )
  return( list( slr, authors) )
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
  extension <- tools::file_ext( filename )
  if (extension == "bib") { return( read.bib( filename, output ) )  }
  else if (extension == "xlsx") { return( readxl::read_excel( filename ) ) }
  else {  message("\nInvalid file type. Use xlsx or bib files.\n") ; return( NULL )}
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
generate_slr_components <- function( articles, authors, year.above = 1900, output = ".", save.pdf = TRUE)
{

  # Latex functions
  slR::write_comments( articles, output = output.folder )
  slR::write_authors( authors, output = output.folder )
  slR::write_institutions( authors, output = output.folder )
  slR::write_countries( authors, output = output.folder )
  slR::write_continents( authors, output = output.folder )
  slR::write_articles( articles, output = output.folder )
  slR::write_graphics( output = output.folder )


  # Plot functions
  slR::score_per_year_boxplot( articles, year.above, output, save.pdf )
  slR::score_per_year_barchart( articles, year.above, output, save.pdf )
  slR::score_per_venue_barchart( articles, year.above, output, save.pdf )
  slR::score_per_author_barchart( articles, year.above, output, save.pdf )
  slR::score_per_publication_barchart( articles, year.above, output, save.pdf )

}




