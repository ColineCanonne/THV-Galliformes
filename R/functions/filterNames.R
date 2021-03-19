filterNames <- function(fill_data, search_data,
                        first_fill_column, last_fill_column) {
  # first_fill_column premiere colonne a remplir
  # last_fill_column derniere colonne a remplir
  # les colonnes doivent etre ds le meme ordre que celle de search_data

  suppr_row <- NULL
  
  # initialisation affichage progression
  pb <- txtProgressBar(min = 0, max = nrow(fill_data), style = 3)
  
  for (i in seq_len(nrow(fill_data))) {
    
    # affichage progression
    setTxtProgressBar(pb, i)
    
    access_num_index <- which(colnames(search_data) == "access_num")
    rows <- which(grepl(fill_data$access_num[i], search_data[, access_num_index]))
    
    if (length(rows) == 0) {
      suppr_row <- c(suppr_row, i) # suppression des articles de fill_data pour lesquels aucun taxon n'a ete trouve
    
    } else {
      tab <- search_data[rows, ]
      
      if (nrow(tab) > 1) {
        
        for (j in seq_len(nrow(tab))) {
          
          other_names <- NULL
          first_search_column <- which(colnames(tab) == "superkingdom")
          last_search_column <- which(colnames(tab) == "species")
          intermediate_tab <- tab[-j, first_search_column:last_search_column]
          
          for (l in seq_len(length(intermediate_tab))) {
            
            for (m in seq_len(nrow(intermediate_tab))) {
              other_names <- c(other_names, intermediate_tab[m, l])
            } # m
          } # l
          
            
          if (!tab$matched_canonical[j] %in% other_names) {
            index <- which(colnames(search_data) == "matched_canonical")
            
            if (is.na(fill_data$matched_canonical[i])) {
              
              for (column_index in first_fill_column:last_fill_column) {
                fill_data[i, column_index] <- as.character(tab[j, index][[1]])
                index <- index + 1
              } # column_index
              
            } else {
              
              for (column_index in first_fill_column:last_fill_column) {
                fill_data[i, column_index] <- paste0(fill_data[i, column_index],"&",
                                                     as.character(tab[j, index][[1]]))
                index <- index + 1
              } # column_index
            }
          }
        } # j
        
      } else {
        index <- which(colnames(search_data) == "matched_canonical")
        
        for (column_index in first_fill_column:last_fill_column) {
          fill_data[i, column_index] <- as.character(tab[1, index][[1]])
          index <- index + 1
        } # column_index
      }
    }
  } # i
  
  if (length(suppr_row) != 0) {
    fill_data <- fill_data[-suppr_row, ]
  }
  
  close(pb)
  return(fill_data)
}
