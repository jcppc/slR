source("R/setup.R")

#' Title
#'
#' @param filename the filename for the SLR data
#'
#' @return
#' @export
#'
#' @examples
read <- function( filename ) {

  slr <-  readxl::read_excel( filename )

  #slr_new <-  methods::setRefClass("SLR", slots = list(name="character", age="numeric", role="character") )
  #new("SLR", name = "Peter", age = 21, role = "Developer")

  return(slr)

  #print("slR v.0.0.0.9000")
  #print("Creator: Joao Caldeira")

}





