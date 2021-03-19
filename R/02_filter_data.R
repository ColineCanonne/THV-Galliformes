#################################################
#
# Cross-taxa population monitoring bibliometrics
#
# 02_filter_data.R
#
# jan.perret@cefe.cnrs.fr
#################################################


###########
### Pre-tri
###########

### Suppression des articles qui ont ete selectionnes dans WOS grace aux keywords plus

# Creation du texte dans lequel cherche WOS (sans les keywords plus)
for (i in seq_len(nrow(corpus))) {
  corpus$text_mixed[i] <- paste0(corpus$title[i], corpus$abstract[i], corpus$keywords[i])
  # Remplacement des tirets par des espaces
  corpus$text_mixed[i] <- gsub(pattern = "-", replacement = " ", x = corpus$text_mixed[i])
}

# Remplissage de la colonne search_voc
if (folder == "monitoring") {
  # Suppression des articles qui ont "population" uniquement dans les keywords plus
  corpus <- corpus[grepl("population", corpus[, which(colnames(corpus) == "text_mixed")]), ]
  
  voc_list <- c("monitor", "dynamic", "demograph", "trend", "viability analysis")

} else if (folder == "time_to_detection") {
  voc_list <- c(gsub("_", " ", folder), "time of detection")

} else if (folder == "cmr") {
  voc_list <- c("capture", "resight", "mark recover", "multiple systems estimation",
                "band recovery", "petersen method", "lincoln method", "closed capture",
                "robust design", "multievent model", "jolly seber", "removal model",
                "tag recovery", "barker model")

} else {
  voc_list <- c(gsub("_", " ", folder))
}


for (voc in voc_list) {
  
  for (i in seq_len(nrow(corpus))) {
    
    if (grepl(voc, corpus$text_mixed[i])) {
      
      if (is.na(corpus$search_voc[i])) {
        corpus$search_voc[i] <- voc
      } else {
        corpus$search_voc[i] <- paste0(corpus$search_voc[i], " ", voc)
      }
    }
  }
}

# Suppression des articles sans mot de recherche
corpus <- corpus[!is.na(corpus$search_voc), ]

# Suppresion des articles de 2020
corpus <- corpus[corpus$year != "2020", ]

# Enregistrement du corpus initial
corpus_initial <- corpus
saveRDS(corpus_initial, file = paste0("./output/text/", folder, "/corpus_initial.rds"))
recap_tab[1, column] <- nrow(corpus)

############
### Filtrage
############

# 1. Suppression des articles publies avant 1991 et ceux dont l'année n'est pas renseignée
annees_conservees <- c(as.character(date:2019))
corpus <- corpus[corpus$year %in% annees_conservees, ]
saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_1.rds"))
recap_tab[2, column] <- nrow(corpus)

# 2. Suppression des articles sans abstract
corpus <- corpus[corpus$abstract != "  NA  ", ] # besoin de deux espaces avant et apres NA
saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_2.rds"))
recap_tab[3, column] <- nrow(corpus)

# 3. Suppressions des categories hors sujet meme si les journeaux sont aussi associes a de bonnes categories
# Creation de la liste de categories a partir du tableau filter_words
WOScategories_list <- keywordsList(filter_words, "WOScategories")
corpus <- filterKeywords(corpus, "WOS_categories", WOScategories_list)
saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_3.rds"))
recap_tab[4, column] <- nrow(corpus)

# 4. Suppression des journaux hors sujet malgre une bonne categorie
# Creation de la liste de journaux a partir du tableau filter_words
journal_list <- keywordsList(filter_words, "journal")
corpus <- filterKeywords(corpus, "journal", journal_list)
saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_4.rds"))
recap_tab[5, column] <- nrow(corpus)

