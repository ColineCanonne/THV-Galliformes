# modified function from github.com/ahasverus/rscimap to load more labels from WOS export files

extractTags_mod <- function(data, tag, sep = "", collapse = TRUE) {
  
  tag <- toupper(tag)
  if (nchar(tag) == 2) tag <- paste(tag, " ", sep = "")
  
  pos <- which(substr(data, 1, 3) == tag)
  if (length(pos) == 0) {
    if (tag == "VL ") {
      tag <- ""
    } else {
      if (tag == "BP ") {
        tag <- "AR "
        pos <- which(substr(data, 1, 3) == tag)
        if (length(pos) == 0) {
          tag <- ""
        }
      } else {
        tag <- ""
      }
    }
  }
  
  if (tag != "") {
    val <- gsub(tag, "", data[pos])
    # modified here to get text without passing everything to upper case
    pos <- pos + 1
    
    while (substr(data[pos], 1, 3) == "   ") {
      
      if (collapse) {
        val <- paste(val, data[pos], sep = sep) # and here
      }
      
      else {
        val <- c(val, data[pos]) # and here
      }
      pos <- pos + 1
    }
    val <- gsub("   ", "", val)
  }
  
  else {
    val <- ""
  }
  
  return(val)
}