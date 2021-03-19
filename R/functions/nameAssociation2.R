
### notes generales sur la fonction :
# la valeur de "command" depend du systeme d'exploitation : 
# shell() pour Windows et system() pour Mac

nameAssociation4 <- function(command, search_data, names_tab) {
  
  # on change le repertoire de travail pour se placer dans le dossier "./gnfinder"
  # et en sortie de fonction, le repertoire de travail original sera retabli
  orig_wd <- getwd()
  on.exit(setwd(orig_wd))
  setwd("./gnfinder")
  
  # initialisation affichage progression
  pb <- utils::txtProgressBar(min = 0, max = nrow(search_data), style = 3)
  
  for (i in seq_len(nrow(search_data))) {
 
    setTxtProgressBar(pb, i) # affichage progression
    
    
    if (!is.na(search_data$text_mixed[i])) { # si text_mixed n'est pas vide
      
      # on ecrit le text_mixed dans un fichier TXT
      write_file(as.vector(search_data$text_mixed[i]), file = "./current_text_mixed.txt", append = FALSE)
      
    } else { # sinon on vide le fichier texte pour qu'aucun nom ne puisse y etre trouve
      
      write_file("there_is_no_text_for_this_article", file = "./current_text_mixed.txt", append = FALSE)
      
    }
    
    # nouvelle commande qui envoi le fichier TXT a gnfinder
    command("gnfinder find -c -l eng current_text_mixed.txt > current_text_mixed_results.json")
    
    # lecture et conversion en liste du fichier JSON
    extracted_names_df <- jsonlite::fromJSON("./current_text_mixed_results.json")
    
    # si aucun nom n'est trouve on passe a l'iteration suivante de la boucle
    if (extracted_names_df$metadata$total_names == 0) {
      next
    }

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
                                         matched_canonical = extracted_names_df$verification$BestResult$matchedCanonical,
                                         superkingdom = NA,
                                         kingdom = NA,
                                         phylum = NA,
                                         subphylum = NA,
                                         superclass = NA,
                                         class = NA,
                                         subclass = NA,
                                         infraclass = NA,
                                         superorder = NA,
                                         order = NA,
                                         suborder = NA,
                                         infraorder = NA,
                                         superfamily = NA,
                                         family = NA,
                                         subfamily = NA,
                                         tribe = NA,
                                         genus = NA,
                                         species = NA,
                                         infraspecies = NA,
                                         access_num = NA)
        
      } else { # s'il y a au moins une hierarchie taxo renseignee
        extracted_names_df <- data.frame(name = extracted_names_df$name,
                                         matched_name = extracted_names_df$verification$BestResult$matchedName,
                                         classification_rank = extracted_names_df$verification$BestResult$classificationRank,
                                         classification_path = extracted_names_df$verification$BestResult$classificationPath,
                                         source_title = extracted_names_df$verification$BestResult$dataSourceTitle,
                                         current_name = extracted_names_df$verification$BestResult$currentName,
                                         match_type = extracted_names_df$verification$BestResult$matchType,
                                         matched_canonical = extracted_names_df$verification$BestResult$matchedCanonical,
                                         superkingdom = NA,
                                         kingdom = NA,
                                         phylum = NA,
                                         subphylum = NA,
                                         superclass = NA,
                                         class = NA,
                                         subclass = NA,
                                         infraclass = NA,
                                         superorder = NA,
                                         order = NA,
                                         suborder = NA,
                                         infraorder = NA,
                                         superfamily = NA,
                                         family = NA,
                                         subfamily = NA,
                                         tribe = NA,
                                         genus = NA,
                                         species = NA,
                                         infraspecies = NA,
                                         access_num = NA)
      }
      
      # transformation des chaines de caracteres dans les colonnes de classification en liste
      # la fragmentation est faite au niveau des traits verticaux
      extracted_names_df$classification_rank <- strsplit(as.character(extracted_names_df$classification_rank), "|", fixed = TRUE)
      extracted_names_df$classification_path <- strsplit(as.character(extracted_names_df$classification_path), "|", fixed = TRUE)
      
      for (j in seq_len(nrow(extracted_names_df))) {
        
        # si le nom est deja dans names_tab on ajoute le access_num de l'article i en face du nom
        if (extracted_names_df$matched_canonical[j] %in% names_tab$matched_canonical) { # si le nom est deja dans names_tab
          row_index2 <- which(names_tab$matched_canonical == as.character(extracted_names_df$matched_canonical[j][[1]]))
          names_tab$access_num[row_index2] <- paste0(names_tab$access_num[row_index2], ",", as.character(search_data$access_num[i]))
          
        } else {
          # ligne du tableau a remplir
          row_index <- which(is.na(names_tab$name))[1]
          
          extracted_names_df$access_num[j] <- as.character(search_data$access_num[i])
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
          
          for (column in seq_len(length(names_tab))){
            
            names_tab[row_index, column] <- as.character(extracted_names_df[j, column])
          }
          
          # saveRDS(object = names_tab, file = "./output/text/names_tab_intermediate.rds")
        }
      }
    }
  }
  
  #suppression des lignes de NA
  names_tab <- names_tab[-which(is.na(names_tab$name)), ]
  
  close(pb)
  return(names_tab)
}


# ### exemple reproductible
# 
# # donnees sources
# latin_corpus <- read.csv2(file = "./gnfinder/latin_corpus_power_analysis.csv")
# latin_corpus <- latin_corpus[c(1:10), ]
# 
# # Initialisation tableau output (10 fois plus grand que latin_corpus)
# names_tab <- data.frame(name = rep(NA, 10*nrow(latin_corpus)), matched_name = NA,
#                         classification_rank = NA, classification_path = NA,
#                         source_title = NA, current_name = NA, match_type = NA, matched_canonical = NA,
#                         superkingdom = NA, kingdom = NA, phylum = NA, subphylum = NA,
#                         superclass = NA, class = NA, subclass = NA, infraclass = NA,
#                         superorder = NA, order = NA, suborder = NA, infraorder = NA,
#                         superfamily = NA, family = NA, subfamily = NA, tribe = NA,
#                         genus = NA, species = NA, infraspecies = NA, access_num = NA)
# 
# 
# # nouvelle fonction avec passage par fichier TXT
# names_tab_4 <- nameAssociation4(command = shell,
#                               search_data = latin_corpus,
#                               names_tab = names_tab)


