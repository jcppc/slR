
getStudiesList <- function( slr, table, dimension )
{
    for (i in 1:length(unique(slr[[dimension]]))) {
      tempValue <- sort(unique(slr[[dimension]]))
      list <- unique(subset(slr$Study, slr[[dimension]] == tempValue[i]))
      for (j in 1:length(list)) { print(j); list[[j]] <- paste0("[",list[j],"]") }
      studies <- paste0(paste(list,collapse=", "), "\\\\[0.2cm]")
      table[["studies"]][table[["names"]] == tempValue[i]] <- studies
    }
  return(table)
}

