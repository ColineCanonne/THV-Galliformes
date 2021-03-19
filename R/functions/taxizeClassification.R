taxizeClassification <- function(data) {
  
  suppr_row <- NULL
  #initialisation affichage progression
  pb <- txtProgressBar(min = 0, max = nrow(data), style = 3)
  
  for (i in seq_len(nrow(data))) {
    #affichage progression
    setTxtProgressBar(pb, i)
    
    if (is.na(data$class[i])) {
      
      if (is.na(data$kingdom[i])) {
        
        if (is.na(data$order[i])) {
          #on considere que la classification est trop incomplete
          id_ncbi <- get_uid(as.character(data$matched_canonical[[i]]),
                             messages = F,
                             ask = F)
          #matched_canonical plutot que name
          #message=FALSE evitera que les resultats s'inscrivent dans la console
          #ask=F renvoie NA si plusieurs matches au lieu de demander a l'utilisateur
          
          if (!is.na(id_ncbi)) {
            #s'il y a un identifiant ncbi, on recupere la classification
            classif <- classification(id_ncbi)
            
            for (Rank in classif[[1]][[2]]) {
              
              if (Rank %in% colnames(data)) {
                column_index <- which(colnames(data) == Rank)
                value_index <- which(classif[[1]][[2]] == Rank)
                data[i, column_index] <- classif[[1]][[1]][[value_index]]
              }
            }
          }
          
          else {
            suppr_row <- c(suppr_row, i)
          }
        }
      }
    }
  }
  
  #suppression des noms mal classifies car non attribuables
  if (length(suppr_row) != 0) {
    data <- data[-suppr_row, ]
  }
  
  close(pb)
  return(data)
}