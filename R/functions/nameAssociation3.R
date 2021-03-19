### notes generales sur la fonction :
# la valeur de "command" depend du systeme d'exploitation : shell() pour Windows et system() pour Mac
# search_data = latin_corpus
# Exemples pour les tests : (dans WOS_files_merged_example_data.txt)
#  i = 2 # aucun nom trouve
#  i = 3 # 2 noms identiques
#  i = 4 # 3 noms differents
#  i = 21 # 2 NoMatchs
#  i = 35 # 6 noms dont 1 NoMatch


nameAssociation3 <- function(command, search_data) {
  
  names_tab <- NULL # initialisation tableau output
  pb <- utils::txtProgressBar(min = 0, max = nrow(search_data), style = 3) # initialisation affichage progression
  
  for (i in seq_len(nrow(search_data))) {
    
    setTxtProgressBar(pb, i) # affichage progression
    
    extracted_names <- command(paste0('cd gnfinder && echo "', search_data$text_mixed[i], '" | gnfinder find -c -l eng'), intern = TRUE)
    
    # si aucun nom n'est trouve on passe a l'iteration suivante de la boucle
    if (length(extracted_names) <= 13) {
      next
    }
    
    # conversion en liste
    extracted_names_df <- jsonlite::fromJSON(extracted_names)
    
    # suppression de la rubrique avec les metadonnees et de la colonne "odds_details"
    extracted_names_df <- extracted_names_df$names[-5]
    
    # condition declenchee si au moins un des noms a pu etre verifie
    if (length(which(extracted_names_df$verification$BestResult$matchType == "NoMatch")) != nrow(extracted_names_df)) { 
      
      # suppression des noms qui n'ont pas pu etre verifies
      extracted_names_df <- subset(extracted_names_df, extracted_names_df$verification$BestResult$matchType != "NoMatch")
      
      # suppression des doublons si un meme nom est present plusieurs fois dans le texte
      doublons <- which(duplicated(extracted_names_df$verification$BestResult$matchedCanonical))
      if (length(doublons) != 0) {
        extracted_names_df <- extracted_names_df[-doublons, ]
      }
      
      # on garde que les colonnes interessantes
      if (length(extracted_names_df$verification$BestResult$classificationRank) == 0) { # condition declenchee s'il n'y a pas la hierarchie taxo
        extracted_names_df <- data.frame(name = extracted_names_df$name,
                                         matched_name = extracted_names_df$verification$BestResult$matchedName,
                                         classification_rank = NA,
                                         classification_path = NA,
                                         source_title = extracted_names_df$verification$BestResult$dataSourceTitle,
                                         current_name = extracted_names_df$verification$BestResult$currentName,
                                         match_type = extracted_names_df$verification$BestResult$matchType,
                                         matched_canonical = extracted_names_df$verification$BestResult$matchedCanonical)
        
      } else { # s'il y a au moins une hierarchie taxo renseignee
        extracted_names_df <- data.frame(name = extracted_names_df$name,
                                         matched_name = extracted_names_df$verification$BestResult$matchedName,
                                         classification_rank = extracted_names_df$verification$BestResult$classificationRank,
                                         classification_path = extracted_names_df$verification$BestResult$classificationPath,
                                         source_title = extracted_names_df$verification$BestResult$dataSourceTitle,
                                         current_name = extracted_names_df$verification$BestResult$currentName,
                                         match_type = extracted_names_df$verification$BestResult$matchType,
                                         matched_canonical = extracted_names_df$verification$BestResult$matchedCanonical)
      }
      
      # transformation des chaines de caracteres dans les colonnes de classification en liste
      # la fragmentation est faite au niveau des traits verticaux
      extracted_names_df$classification_rank <- strsplit(as.character(extracted_names_df$classification_rank), "|", fixed = TRUE)
      extracted_names_df$classification_path <- strsplit(as.character(extracted_names_df$classification_path), "|", fixed = TRUE)
      
      # si rien n'a encore ete ajoute a names_tab
      if (length(names_tab) == 0) {
        
        # ajout des colonnes de classification et access_num
        names_tab <- data.frame(extracted_names_df, superkingdom = NA, kingdom = NA, phylum = NA,
                                subphylum = NA, superclass = NA, class = NA, subclass = NA,
                                infraclass = NA, superorder = NA, order = NA, suborder = NA,
                                infraorder = NA, superfamily = NA, family = NA, subfamily = NA,
                                tribe = NA, genus = NA, species = NA, infraspecies = NA,
                                access_num = NA)
        
        for (j in seq_len(nrow(extracted_names_df))) {
          names_tab$access_num[j] <- search_data$access_num[i]
          
          if (length(names_tab$classification_rank[j][[1]]) == length(names_tab$classification_path[j][[1]])) {
            
            # les listes path et rank doivent etre de meme longueur pour qu'il n'y ait pas de decallage dans l'assignation
            for (Rank in names_tab$classification_rank[j][[1]]) {
              
              # Rank est une chaine de caractere qui peut valoir "kingdom", "phylum"... selon les valeurs du tableau
              if (!is.na(Rank) & Rank != "") {
                if (Rank %in% colnames(names_tab)) {
                  column_index <- which(colnames(names_tab) == Rank) #  numero de la colonne de names_tab dont le nom est identique a la valeur de Rank
                  value_index <- which(extracted_names_df$classification_rank[j][[1]] == Rank)
                  # value_index est l'indice de Rank dans la liste classificationRank du tableau extracted_names_df 
                  # qui est identique a l'indice de la valeur correspondante dans la liste classificationPath
                  names_tab[j, column_index] <- extracted_names_df$classification_path[j][[1]][value_index][1]
                  # la case de ligne i et de colonne Rank de names_tab prend la valeur d'indice value_index dans la liste classificationPath
                }
              }
            }
          }
        }
      } else {
        
        extracted_names_df <- data.frame(extracted_names_df, superkingdom = NA, kingdom = NA, phylum = NA,
                                         subphylum = NA, superclass = NA, class = NA, subclass = NA,
                                         infraclass = NA, superorder = NA, order = NA, suborder = NA,
                                         infraorder = NA, superfamily = NA, family = NA, subfamily = NA,
                                         tribe = NA, genus = NA, species = NA, infraspecies = NA,
                                         access_num = NA)
        
        for (j in seq_len(nrow(extracted_names_df))) {
          
          # si le nom est deja dans names_tab on ajoute le access_num de l'article i en face du nom
          if (extracted_names_df$name[j] %in% names_tab$name) { # si le nom est deja dans names_tab
            row_index <- which(names_tab$name == as.character(extracted_names_df$name[j][[1]]))
            names_tab$access_num[row_index] <- paste0(names_tab$access_num[row_index], ",", search_data$access_num[i])
            
          } else {
            extracted_names_df$access_num[j] <- search_data$access_num[i]
            if (length(extracted_names_df$classification_rank[j][[1]]) == length(extracted_names_df$classification_path[j][[1]])) {
              
              # les listes path et rank doivent etre de meme longueur pour qu'il n'y ait pas de decallage dans l'assignation
              for (Rank in extracted_names_df$classification_rank[j][[1]]) {
                
                # Rank est une chaine de caractere qui peut valoir "kingdom", "phylum"... selon les valeurs du tableau
                if (!is.na(Rank) & Rank != "") {
                  if (Rank %in% colnames(extracted_names_df)) {
                    column_index <- which(colnames(extracted_names_df) == Rank)
                    # column_index est le numero de la colonne de extracted_names_df dont le nom est identique a la valeur de Rank
                    value_index <- which(extracted_names_df$classification_rank[j][[1]] == Rank)
                    # value_index est l'indice de Rank dans la liste classificationRank du tableau extracted_names_df 
                    # qui est identique a l'indice de la valeur correspondante dans la liste classificationPath
                    extracted_names_df[j, column_index] <- extracted_names_df$classification_path[j][[1]][value_index][1]
                    # la case de ligne i et de colonne Rank de extracted_names_df prend la valeur d'indice value_index dans la liste classificationPath
                  }
                }
              }
            }
            # on append names_tab avec les infos de article numero i
            names_tab <- rbind(names_tab, extracted_names_df[j, ])
            saveRDS(object = names_tab, file = "./output/text/names_tab_intermediate.rds")
          }
        }
      }
    }
  }
  
  close(pb)
  return(names_tab)
}