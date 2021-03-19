taxaAssociation <- function(data, search_column, fill_column, taxa_list, name_taxa) {
  #search_column est un vecteur contenant le ou les noms des colonnes d'interet
  #fill_column est une chaine de caracteres donnant le nom de la colonne a remplir
  #taxa_list est la liste contenant les mots clefs associes a un taxon
  #name_taxa est une chaine de caracteres donnant le nom du taxon en question
  
  search_column_index <- c(which(colnames(data) %in% search_column))
  fill_column_index <- c(which(colnames(data) == fill_column))
  
  for (i in search_column_index) {
    
    for (j in seq_len(length(taxa_list))) {
      row_index <- c(which(grepl(taxa_list[j], data[, i])))
      #row_index contient tous les numeros de lignes  associees au taxon
      
      for (k in row_index) {
        
        if (is.na(data[k, fill_column_index])) {
          data[k, fill_column_index] <- name_taxa
          #NA est remplace par le nom du taxon
        }
        
        else if (!grepl(name_taxa, data[k, fill_column_index])) {
          data[k, fill_column_index] <- paste0(data[k, fill_column_index],
                                               " ", name_taxa)
          #un autre taxon etant deja renseigne, le nouveau taxon s'ajoute
        }
        
        #si le taxon etait deja assigne a l'article, rien n'est fait
      }
    }
  }
  
  return(data)
}
