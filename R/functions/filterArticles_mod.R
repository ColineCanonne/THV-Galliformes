# modified function from github.com/ahasverus/rscimap to load more labels from WOS export files

filterArticles_mod <- function(files,
                               type = c("Article",
                                        "Editorial Material",
                                        "Review"),
                               save_folder) {
  
  options(warn = -1)
  
  if (!is.null(type)) {
    dt <- paste(paste("DT", type), collapse = "|")
  }
  
  else {
    dt <- "DT "
  }
  
  cat(paste0("\n\n>>> Filtering citations < ",
             paste0(type, collapse = ", "),
             " >...\n"))
  
  refs <- list()
  k <- 1
  
  for (i in seq_len(length(files))) {
    
    tab  <- readLines(files[i])
    
    # field tag for the start of each record
    pos0 <- which(substr(tab, 1, 3) == "PT ")
    # field tag for the end of each record
    pos1 <- which(substr(tab, 1, 2) == "ER")
    
    for (j in seq_len(length(pos0))) {
      
      zero <- ifelse(nchar(k) == 1, "000",
                     ifelse(nchar(k) == 2, "00",
                            ifelse(nchar(k) == 3, "0", "")))
      dat  <- paste(c(tab[pos0[j]:pos1[j]], ""), collapse = "\n", sep = "")
      
      if (length(grep(dt, dat)) > 0) {
        
        mat <- tab[pos0[j]:pos1[j]]
        
        infos  <- list()
        labels <- c("UT", "DI", "AU", "TI", "AB", "DE", "PY", "SO", "WC")
        
        for (z in seq_len(length(labels))) {
          
          infos[[z]] <- extractTags_mod(mat, tag = labels[z],
                                    sep = ifelse(labels[z] == "AU", " ; ", " "),
                                    collapse = ifelse(labels[z] == "CR",
                                                      FALSE,
                                                      TRUE))
          names(infos)[z] <- labels[z]
        }
        
        infos <- c(NOID = paste("REF", zero, k, sep = ""), infos)
        
        refs[[k]] <- infos
        k <- k + 1
      }
    }
  }
  
  cat(paste0(">>> Done!\n"))
  cat(paste0(">>> Results exported in ",
             save_folder,
             "/analysis/articles.rds\n\n"))
  
  dir.create(paste0(save_folder, "/analysis"), showWarnings = FALSE)
  saveRDS(refs, paste0(save_folder, "/analysis/articles.rds"))
  
  return(refs)
}
