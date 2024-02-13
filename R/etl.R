
getStudiesList <- function(table, dimension, slr_exploded)
{
  for (i in 1:length(unique(slr_exploded[[dimension]]))) {

    tempValue <- sort(unique(slr_exploded[[dimension]]))
    list <- unique(subset(slr_exploded$Study, slr_exploded[[dimension]] == tempValue[i]))
    for (j in 1:length(list)) { list[[j]] <- paste0("[",list[j],"]") }
    studies <- paste0(paste(list,collapse=", "), "\\\\[0.2cm]")
    table[["studies"]][table[["names"]] == tempValue[i]] <- studies
  }
  table
}



frequency <- function( slr, year.above, output, save.pdf, plot.name, dimension )
{

  plot.name <- plot.name
  slr <-  slr$articles
  slr <-  slr[slr$Year > year.above,]

  Dimension <- factor(stringr::str_squish(unlist(strsplit( with(slr, get(dimension)), split=","))))

  slr_exploded <- slr %>% tidyr::separate_rows( dimension , sep = ",")

  SumDimension <- data.frame( group=as.factor(dimension), names = levels(factor(Dimension)), names_latex = paste0("\\textbf{",levels(factor(Dimension)),"}"), values = summary(factor(Dimension)), percent = paste0(round(summary(factor(Dimension))/nrow(slr)*100,2),"\\%"), studies = "", stringsAsFactors = FALSE, row.names = NULL)
  SumDimension <- dplyr::arrange(SumDimension, -values)

  SumDimension = getStudiesList( SumDimension, dimension, slr_exploded )

  write_table_to_latex( SumDimension, rownames = FALSE, output = output, tablename = paste0("Sum",dimension) )

  write_table_header( dimension, output = output )

}
