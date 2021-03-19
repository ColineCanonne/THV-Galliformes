
taxoComplete <- function(tab, first_column, last_column) {

  # minor corrections
  tab[tab == "Metazoa"] <- "Animalia" ##### a gerer autrement ?
  tab[tab == "Not assigned"] <- NA
  tab[tab == ""] <- NA
  tab <- tab[rowSums(is.na(tab[, c(first_column:last_column)])) != length(first_column:last_column), ] # supprime toutes les lignes qui contiennent uniquement des NA
  
  # keep only the columns with the taxonomic hierarchy
  taxo_tab <- tab[, c(first_column:last_column)]
  
  pb <- utils::txtProgressBar(min = 0, max = nrow(taxo_tab), style = 3) # initialisation affichage progression
  
  # loop 1 : browse row by row
  for (i in seq_len(nrow(taxo_tab))) {
    # cat("\n", "i =", i)
    
    setTxtProgressBar(pb, i) # affichage progression
    
    # loop 2 : browse column by column
    for (j in rev(seq_len(ncol(taxo_tab)))) {
      # cat("\n", "i =", i, "; j =", j)

      # si la case n'est pas vide on stocke la hierarchie taxo superieure et on lance la 3eme boucle
      if (!is.na(taxo_tab[i, j])) {
        
        taxa_name <- taxo_tab[i, j]
        taxo_list <- taxo_tab[i, c(1:(j-1))]

        if (all(is.na(taxo_list))) {
          break
        }
        
        # loop 3 : browse through the column j
        for (k in seq_len(nrow(taxo_tab))) {
          # cat("\n", "i =", i, "; j =", j, "; k = ", k)
          
          # si un nom identique a taxa_name est trouve on remplace les NA precedents
          if (!is.na(taxo_tab[k,j])) {
            
            if (taxo_tab[k,j] == taxa_name) {
              
              if (all(is.na(taxo_tab[k, c(1:(j-1))]))) { # s'il y a que des NA on remplace tout

                taxo_tab[k, c(1:(j-1))] <- taxo_list

              } else if (identical(taxo_tab[k, c(1:(j-1))], taxo_list)) { # si vecteurs identiques on passe a l'iteration k+1

                next
                
              } else if (all(taxo_tab[k, c(1:(j-1))] == taxo_list, na.rm = TRUE)) { # si tous les noms presents dans les deux vecteurs sont identiques

                # index des valeurs qui sont NA a gauche et pas a droite
                replace_vect <- which(is.na(taxo_tab[k, c(1:(j-1))]) & !is.na(taxo_list))

                if (length(replace_vect) != 0) {
                  taxo_tab[k, replace_vect] <- taxo_list[replace_vect]
                }
                
              }
            
            }
            
          }
          
        } # for k
        
      }
      
    } # for j
    
  } # for i

  # assign the values from the columns with the taxonomic hierarchy we just filled to the initial dataframe
  tab[, c(first_column:last_column)] <- taxo_tab
  
  close(pb)
  return(tab)
}
