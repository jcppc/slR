
write_table_to_latex <- function( table, rownames = TRUE, output = ".", tablename)
{
  if(missing( tablename ))
  {
    tablename <- deparse(substitute(table))
  }

  filename <- paste0( output ,"/", tablename, ".tex")
  file.create( filename )
  fileConn <- file( filename )
  utils::write.table(table[,-c(1,2)], file = filename, sep = " & ", row.names = rownames, col.names = FALSE, quote = FALSE)
  close(fileConn)
}


#' Writes any data table into latex format
#'
#' @param table The data table to render in latex
#' @param rownames Include or not the row names
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_authors <- function( table, rownames = FALSE, output = ".")
{
  table <-  table$authors
  n_studies <- length(unique(table$Study))
  table$Study <- paste0("[", table$Study , "]")
  studies <- data.frame( studies = stats::aggregate(Study ~ Author, data = table, toString)[2])
  colnames(studies)[1] <- c("studies")
  authors <- data.frame(  group=as.factor("Authors"), names = levels(factor(table$Author)), names_latex = paste0("\\textbf{",levels(factor(table$Author)),"}"),   values = summary(factor(table$Author), maxsum = length(unique(table$Author))), percent = paste0(round(summary(factor(table$Author), maxsum = length(unique(table$Author)))/n_studies*100,2),"\\%"), studies = studies,  stringsAsFactors = FALSE, row.names = NULL)
  authors <- dplyr::arrange(authors, -values)
  authors$studies <- paste0(authors$studies, '\\\\')
  write_table_to_latex( authors, rownames, output )
}


#' Title
#'
#' @param table The data table to render in latex
#' @param rownames Include or not the row names
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_institutions <- function( table, rownames = FALSE, output = ".")
{
  table <-  table$authors
  table$Study <- paste0("[", table$Study , "]")
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Institution","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Institution, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  institutions <- data.frame(  group=as.factor("Institutions"), names = levels(factor(dataset$Institution)), names_latex = paste0("\\textbf{",levels(factor(dataset$Institution)),"}"),   values = summary(factor(dataset$Institution)), percent = paste0(round(summary(factor(dataset$Institution))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  institutions <- dplyr::arrange(institutions, -values)
  institutions$studies <- paste0(institutions$studies, '\\\\')
  write_table_to_latex( institutions, rownames, output )
}


#' Title
#'
#' @param table The data table to render in latex
#' @param rownames Include or not the row names
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_countries <- function( table, rownames = FALSE, output = ".")
{
  table <-  table$authors
  table$Study <- paste0("[", table$Study , "]")
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Country","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Country, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  SumCountriesTemp <- data.frame(  group=as.factor("Countries"), names = levels(factor(dataset$Country)), names_latex = paste0("\\textbf{",levels(factor(dataset$Country)),"}"),   values = summary(factor(dataset$Country)), percent = paste0(round(summary(factor(dataset$Country))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  SumCountriesTemp <- dplyr::arrange(SumCountriesTemp, -values)
  SumCountriesTemp$studies <- paste0(SumCountriesTemp$studies, '\\\\')
  countries <- SumCountriesTemp
  write_table_to_latex( countries, rownames, output )
}


#' Title
#'
#' @param table The data table to render in latex
#' @param rownames Include or not the row names
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_continents <- function( table, rownames = FALSE, output = ".")
{
  table <-  table$authors
  table$Study <- paste0("[", table$Study , "]")
  n_studies <- length(unique(table$Study))
  dataset <- unique(table[c("Continent","Study")])
  studies <- data.frame( studies = stats::aggregate(Study ~ Continent, data = dataset, toString)[2])
  colnames(studies)[1] <- c("studies")
  SumContinentTemp <- data.frame(  group=as.factor("Continents"), names = levels(factor(dataset$Continent)), names_latex = paste0("\\textbf{",levels(factor(dataset$Continent)),"}"),   values = summary(factor(dataset$Continent)), percent = paste0(round(summary(factor(dataset$Continent))/n_studies*100,2),"\\%"), studies = studies, stringsAsFactors = FALSE, row.names = NULL)
  SumContinentTemp <- dplyr::arrange(SumContinentTemp, -values)
  SumContinentTemp$studies <- paste0(SumContinentTemp$studies, '\\\\')
  continents <- SumContinentTemp
  write_table_to_latex( continents, rownames, output )
}


#' Write articles comments into a latex file, including the studi identifier and citation reference
#'
#' @param slr The SLR data object to plot data from
#' @param rownames Flag to include rownames
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_comments <- function( slr , rownames = TRUE, output = ".")
{
  slr <-  slr$articles
  filename <- paste0( output , "/", "comments.tex")
  file.create(filename)
  fileConn <- file(filename)
  comments <- paste0("\\noindent[\\textbf{",slr$Study,"}] ",gsub('.{1}$','',slr$Comments), paste0(" \\cite{",slr$Citation,"}.") )
  comments <- gsub("%", "\\\\%", comments )
  comments <- gsub("~", "$\\\\sim$", comments )
  writeLines(paste0( comments ,'\\\\[0.1cm]\n'), fileConn)
  close(fileConn)
}

#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_articles <- function( slr , output = ".") {
  slr <-  slr$articles
  filename <- paste0( output, "/" , "articles.tex")
  file.create(filename)
  fileConn <- file(filename)
  #Study <- paste0(paste0("\\textbf{",slr$Stage,"}~"),paste0("\\label{ref:",slr$Citation,"}"))
  Study <- paste0("\\textbf{",slr$Study,"}")
  writeLines(paste0(paste(Study,slr$Total,slr$Year,slr$Author,slr$Title,slr$Publication, sep=" & "), ' \\\\[0.1cm]'), fileConn)
  close(fileConn)
  #file.show(filename)
}



#' Title
#'
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_graphics <- function( output = "." )
{
  files <- list.files( path = output, pattern = "\\.pdf$")
  latex_file <- paste0( output, "/", "graphics.tex")
  file.create( latex_file )
  fileLatex <- file( latex_file )
  writeLines(paste0( "\\plot{",files,"}[Put Your Caption Here][12][H][trim=0cm 0cm 0cm 0cm]"), fileLatex )
  close(fileLatex)
}


#' Title
#'
#' @param slr The SLR data object to plot data from
#' @param year.above Filter by year the data on the SLR (default is above 1900)
#' @param output Folder where to write the plots into (default is the current directory - '.')
#' @param save.pdf Whether to save or not the plot to pdf file (default is TRUE)
#' @param plot.name Name of the file to plot data into
#' @param dimension The dimension name to report against
#'
#' @return
#' @export
#'
#' @examples
write_frequency_of_dimension <- function( dimension, slr, year.above = 1900, output = ".", save.pdf = TRUE, plot.name = "new_plot.pdf")
{
  frequency( slr, year.above, output, save.pdf, plot.name, dimension )
}


#' Title
#'
#' @param output Folder where to write the latex files into (default is the current directory - '.')
#'
#' @return
#' @export
#'
#' @examples
write_sample_table_template <- function( output = "." )
{

  latex_file <- paste0( output, "/", paste0(stringr::str_to_title( dimension ),".tex" )  )
  sink( latex_file )

  cat("\\begin{longtable}{p{5.1cm}ccp{7.5cm}} % Don't forget to import package longtable \n")
  cat("\\caption{List of all Contributors} \n")
  cat("\\label{table:SumContributors} \n")
  cat("\\\\\\hline\\noalign{\\smallskip} \n")
  #cat("%\textbf{\\#} & \n")
  cat("\\textbf{Name} & \\textbf{Freq.} & \\textbf{Perc.} & \\textbf{Ref.}\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip} \n")
  cat("\\endfirsthead \n")
  cat("\\multicolumn{4}{c} \n")
  #cat("\\%{ \\tablename\ \\thetable{}: continued from previous page } \\ \n")
  cat("{ \\begin{footnotesize} \\tablename\ \\thetable{}: continued from previous page \\end{footnotesize} } \\\\ \n")
  cat("\\hline\\noalign{\\smallskip} \n")
  #cat("\\%\\textbf{\\#} & \n")
  cat("\\textbf{Name} & \\textbf{Freq.} & \\textbf{Perc.} & \\textbf{Ref.}\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip} \n")
  cat("\\endhead \n")
  cat("\\hline \\multicolumn{4}{r}{{Continued on next page}} \\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip} \n")
  cat("\\endfoot \n")
  cat("\\hline \n")
  cat("\\endlastfoot \n\n")
  cat("\\input{authors} % For other tables, use this template header and just change the file name (authors) here \n\n")
  cat("\\end{longtable}")

  sink()

}


write_table_header <- function( dimension, output = "." )
{
  dimension <- stringr::str_to_title( dimension )
  sum.dimension <-  paste0("Sum",dimension)
  latex_file <- paste0( output, "/", paste0( dimension,".tex" )  )
  sink( latex_file )

  cat("\\begin{table*}[!htbp] \n")
  cat("\\caption{",dimension,"Findings} \n")
  cat("\\label{table:Sum")
  cat(dimension)
  cat("} \n")
  cat("\\centering \n")
  cat("\\begin{tabular}{p{3.5cm}ccp{10cm}} \n")
  cat("\\hline\\noalign{\\smallskip} \n")
  cat("\\textbf{Scope} & \\textbf{Freq.} & \\textbf{Perc.} & \\textbf{Ref.}\\\\ \n")
  cat("\\noalign{\\smallskip}\\hline\\noalign{\\smallskip} \n")
  cat("\\input{",sum.dimension,"} \n")
  cat("\\hline\\noalign{\\smallskip} \n")
  cat("\\end{tabular} \n")
  cat("\\end{table*} \n")

  sink()
}



