#' Title
#'
#' @param filename the filename for the SLR data
#'
#' @return
#' @export
#'
#' @examples
build_slr <- function( filename ) {

  slr <-  readxl::read_excel( filename )

  #slr_new <-  methods::setRefClass("SLR", slots = list(name="character", age="numeric", role="character") )
  #new("SLR", name = "Peter", age = 21, role = "Developer")

  return(slr)

  #print("slR v.0.0.0.9000")
  #print("Creator: Joao Caldeira")

}


#' Title
#'
#' @param table The data table to render in latex
#' @param rownames Include or not the row names
#' @param output Folder to output the latex file
#'
#' @return
#' @export
#'
#' @examples
writeSummaryTable <- function(table, rownames = TRUE, output = ".")
{

    tablename <- deparse(substitute(table))

    #if (latex_render) {

      filename <- paste0( output ,"/", tablename, ".tex")
      file.create( filename )
      fileConn <- file( filename )
      utils::write.table(table[,-c(1,2)], file = filename, sep = " & ", row.names = rownames, col.names = FALSE, quote = FALSE)
      #writeLines(paste(paste(table, sep=" & "), ' \\\\[0.1cm]'), fileConn)
      close(fileConn)
      #file.show(filename)
    #}

}

#' Title
#'
#' @param SLR_Final
#' @param rownames
#' @param output
#'
#' @return
#' @export
#'
#' @examples
writeComments <- function( SLR_Final, rownames = TRUE, output = ".")
{

  filename <- paste0( output , "/", "comments.tex")
  file.create(filename)
  fileConn <- file(filename)
  Study <- paste0("\\noindent[\\textbf{",SLR_Final$Stage,"}] ",gsub('.{1}$','',SLR_Final$Comments), paste0(" \\cite{",SLR_Final$Citation,"}.") )
  #Study <- paste0("\\noindent[\\textbf{",SLR_Final$Stage,"}] ", SLR_Final$Comments)
  Study <- gsub("%", "\\\\%", Study)
  Study <- gsub("~", "$\\\\sim$", Study)
  writeLines(paste0(Study,'\\\\[0.1cm]\n'), fileConn)
  close(fileConn)
  #file.show(filename)

}





