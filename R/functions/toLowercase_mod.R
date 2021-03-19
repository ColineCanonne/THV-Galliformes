# modified function from github.com/ahasverus/rscimap

toLowercase_mod <- function(data, tag = "TI", save_folder) {
  
  if (!is.list(data))
    stop("Data has to be the output of filterArticles(), i.e. a list.")
  
  cat(paste0("\n\n>>> Converting < ", paste0(tag, collapse = ", "),
             " > to lower case...\n"))
  
  if (!is.null(tag)) {
    data <- lapply(data, function(x) {
      for (i in seq_len(length(tag))) {
        x[[tag[i]]] <- tolower(x[[tag[i]]])
      }
      return(x)
    })
  }
  cat(paste0(">>> Done!\n"))
  cat(paste0(">>> Results exported in ", save_folder,
             "/analysis/articles.rds\n\n"))
  dir.create(paste0(save_folder, "/analysis"), showWarnings = FALSE)
  saveRDS(data, paste0(save_folder, "/analysis/articles.rds"))
  return(data)
}