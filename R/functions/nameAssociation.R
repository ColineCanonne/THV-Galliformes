nameAssociation <- function(data, search_columns, fill_columns, name_list){
  #search_columns est un vecteur contenant le ou les noms des colonnes de data ou chercher les mots clefs
  #fill_columns est un vecteur contenant le ou les noms des colonnes de data qu'il faut completer
  #name_list est la liste contenant les differents noms latins du corpus
  column_index1 <- c(which(colnames(data) %in% search_columns))
  column_index2 <- c(which(colnames(data) %in% fill_columns))
  column_index3 <- c(which(colnames(names_tab) %in% fill_columns))
  #column_index3 est le numero des colonnes de names_tab dont il faut tirer les valeurs
  for (i in column_index1) {
    titre <- paste("Avancement de la colonne ", i)#choix du titre pour l'affichage
    print(titre)
    pb <- txtProgressBar(min=0, max=length(name_list), style=3)
    for (names in name_list) {
      row_index <- c(which(grepl(names, data[,i])))
      #row_index contient tous les numeros de lignes associees au nom latin
      #affichage progression
      setTxtProgressBar(pb, which(name_list == names))
      if(length(row_index)!=0){#si le nom latin n'est pas trouve dans le corpus, row_index est vide ce qui pose des problemes dans la fonction
        for (k in row_index){
          if (is.na(data$name[k])){
            data[k,column_index2] <- names_tab[which(paste0(" ",names_tab$name," ")==names),column_index3]
            #si aucun nom latin n'avait ete renseigne, NA est remplace par le nom latin
          }else if(!grepl(names, paste0(" ", data$name[k], " "))){
            j <- column_index3[1]
            for(l in column_index2){
              data[k,l] <- paste0(data[k,l]," , ", names_tab[which(paste0(" ",names_tab$name," ")==names),j])
              #si un autre nom latin avait deja ete renseigne, le nouveau nom s'ajoute
              j <- j+1#possible car les colonnes se suivent dans le tableau
            }
          }#si le nom etait deja assigne a l'article, rien n'est fait
        }
      }
    }
    close(pb)
  }
  return(data)
}