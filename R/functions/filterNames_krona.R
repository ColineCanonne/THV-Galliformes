
filterNames_Krona <- function(fill_data, first_fill_column, last_fill_column) {
  
  # on recupere la liste des valeurs uniques de numeros d'acces
  access_vect <- unique(unlist(strsplit(fill_data$access_num, split = ",")))
  
  rows_to_delete <- c(NULL)
  
  pb <- utils::txtProgressBar(min = 0, max = length(access_vect), style = 3) # initialisation affichage progression
  
  for (i in seq_len(length(access_vect))) { # boucle 1 : parcourt les valeurs uniques de access_vect
    # cat("\n", "i =", i)
    setTxtProgressBar(pb, i) # affichage progression
    
    myaccess <- access_vect[i] # le numero d'acces courant
    rows <- which(grepl(myaccess, fill_data$access_num)) # index des lignes qui contiennent ce numero d'acces
    
    if (length(rows) == 1) { # s'il n'y a pas d'autre ligne avec ce numero d'acces, on passe a l'iteration suivante
      next
    }
    
    mytab <- fill_data[c(rows), ] # sous-tableau avec uniquement ces lignes
    mytab <- mytab[, c(first_fill_column:last_fill_column)] # on garde que les colonnes avec la hierarchie taxo
    
    for (j in seq_len(nrow(mytab))) { # boucle 2 : parcourt le tableau mytab
      # cat("\n", "i =", i, "; j =", j)
      myrow <- mytab[j, ]
      
      for (k in seq_len(nrow(mytab))[! seq_len(nrow(mytab)) %in% j]) { # boucle 2 : reparcourt le tableau mytab sauf la ligne j
        # cat("\n", "i =", i, "; j =", j, "; k = ", k)
        
        compare_row <- mytab[k, ]
        
        # myrow <- c("a", "b", "c")
        # compare_row <- c(NA, "d", NA)
        # myrow == compare_row
        # any(myrow == compare_row)
        # any(myrow == compare_row, na.rm = TRUE)
        # all(myrow == compare_row, na.rm = TRUE)
        
        if (!any(myrow == compare_row, na.rm = TRUE)) { # s'il y a une valeur differente entre les deux vecteurs, on passe a l'iteration suivante
          next
        }
        if (sum(!is.na(myrow)) > sum(!is.na(compare_row))) { # s'il y a plus de valeurs remplies dans myrow que dans comparerow, ca veut dire que compare row est incluse dans myrow
          rows_to_delete <- c(rows_to_delete, k)
          
        }
          
      } # k
      
      # store rownames of the rows_to_delete
      rownames_to_delete <- rownames(mytab[c(rows_to_delete), ])
      
    } # j
    
    # pour les lignes qu'il y a dans rows_to_delete, on supprime le numero d'acces qu'il y a dans myaccess
    row_index <- which(rownames(fill_data) %in% rownames_to_delete)

    fill_data$access_num[c(row_index)] <- gsub(myaccess, "", fill_data$access_num[c(row_index)])
    rows_to_delete <- c(NULL)
    
  } # i
  
  # on nettoie les virgules en tete et queue
  fill_data$access_num <- gsub("^,*|(?<=,),|,*$", "", fill_data$access_num, perl = TRUE)

  # on supprime les lignes sans aucun article assigne
  fill_data <- subset(fill_data, fill_data$access_num != "")
  
  close(pb)
  return(fill_data)
}
