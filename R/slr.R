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





