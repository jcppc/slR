#' Prints the package version and authors' details
#'
#' @return
#' @export
#'
#' @examples
version <- function() {

  version <- paste("Package (slR) version: ", utils::packageVersion("slR"))
  authors <- "\nBuilt by: Jo\u00e3o Caldeira"
  organization <-  "\nBuilt at: Iscte - Instituto Universit\u00e1rio de Lisboa, Portugal\n"

  insight::print_color( version, "green" )
  insight::print_color( authors, "black")
  insight::print_color( organization, "blue")

}




