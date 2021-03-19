addSpace <- function(data, column) {
  #column est un vecteur contenant le ou les noms des colonnes d'interet
  
  if (!is.data.frame(data))
    stop("Data has to be a data frame.")
  
  column_index <- c(which(colnames(data) %in% column))
  
  for (i in seq_len(nrow(data))) {
    
    for (j in column_index) {
      data[i, j] <- paste(" ", data[i, j], " ")
    }
  }
  
  return(data)
}