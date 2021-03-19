# modified function from github.com/ahasverus/rscimap

removePunctuation_mod <- function(data, tag = "TI",
                                  exception = NULL, save_folder) {
  
  if (!is.list(data)) {
    stop("Data has to be the output of filterArticles(), i.e. a list.")
  }
  
  cat(paste0("\n>>> Removing punctuations in < ",
             paste0(tag, collapse = ", "),
             " >...\n"))
  
  
  if (!is.null(tag)) {
    
    if (is.null(exception)) {
      expr <- "[[:punct:]]"
    } 
    
    else {
      expr <- paste("([", paste("", exception, collapse = "", sep = ""),
                    "])|[[:punct:]]", collapse = "", sep = "")
    }
    
    for (i in seq_len(length(tag))) {
      
      data <- lapply(data, function(x) {
        x[tag[i]] <- gsub(expr, "\\1", x[tag[i]])
        return(x)
      })
      
      ### Apostrophe lookup
      data <- lapply(data, function(x) {
        x[tag[i]] <- gsub("\"s |\"", "", x[tag[i]])
        return(x)
      })
    }
  }
  
  cat(paste0(">>> Done!\n"))
  cat(paste0(">>> Results exported in ", save_folder,
             "/analysis/articles.rds\n\n"))
  
  dir.create(paste0(save_folder, "/analysis"), showWarnings = FALSE)
  saveRDS(data, paste0(save_folder, "/analysis/articles.rds"))
  
  return(data)
}