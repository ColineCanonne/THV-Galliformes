filterKeywords <- function(data, column, keywords) {
  #column est un vecteur contenant le ou les noms des colonnes d'interet
  
  column_index <- c(which(colnames(data) %in% column))
  access_num_index <- c(which(colnames(data) == "access_num"))

  for (i in column_index) {
    
    for (j in seq_len(length(keywords))) {
      tab <- data[grepl(keywords[j], data[, i]), ]
      #tab = sous-tableau ne contenant que les articles avec mots clefs
      data <- subset(data, !(data[, access_num_index] %in% tab[, access_num_index]))
      #data prend ici toutes les lignes de data dont les numeros d'acces WOS ne sont pas dans tab
    }
  }
  
  return(data)
}