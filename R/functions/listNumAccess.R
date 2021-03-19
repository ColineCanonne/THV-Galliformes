# name_taxa est une chaine de caracteres donnant le nom du taxon

listNumAccess <- function(data, name_taxa, column_name, list = NULL) {
  
  column_index <- which(colnames(data) == column_name)
  list <- c(list, data$access_num[c(which(grepl(name_taxa,
                                                data[, column_index])))])
  
  return(list)
}
