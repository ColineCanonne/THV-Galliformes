keywordsList <- function(data, column) {
  # column est le nom de la colonne dont on veut extraire la liste de mots clefs
  
  column_index <- c(which(colnames(data) == column))
  keywords_list <- as.character(data[, column_index])
  keywords_list2 <- NULL# initiation de la liste finale
  
  # suppression de la ponctuation sauf "-"
  exception <- "-"
  expr <- paste("([", paste("", exception, collapse = "", sep = ""),
                "])|[[:punct:]]", collapse = "",
                sep = "")
  
  for (i in seq_len(nrow(data))) {
    
    if (keywords_list[i] != "") {
      keywords_list2 <- c(keywords_list2,
                          paste0(" ", gsub(expr, "\\1",
                                           tolower(keywords_list[i])), " "))
      # suppression des elements vides
      # ajout d'espaces avant et apres chaque mot de la liste
    }
  }
  
  return(keywords_list2)
}