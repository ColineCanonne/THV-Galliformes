#################################################
#
# Cross-taxa population monitoring bibliometrics
#
# 01_load_data.R
#
# jan.perret@cefe.cnrs.fr
#################################################


# corpus
raw_corpus <- paste0("./data/raw/", corpus_name, ".txt")

### load corpus
corpus_list <- filterArticles_mod(files = raw_corpus,
                                  type = NULL,
                                  save_folder = "./data/tidy") 
# type = NULL permet de prendre tous les documents qu'il y a dans le fichier .txt

# passage des champs suivants en minuscules : titre, mots clefs, abstract, journal et WOS categorie
corpus_list <- toLowercase_mod(data = corpus_list,
                               tag = c("TI", "DE", "AB", "SO", "WC"),
                               save_folder = "./data/tidy")

# supression de la ponctuation a l'exeption des tirets
corpus_list <- removePunctuation_mod(data = corpus_list,
                                     tag = c("TI", "DE", "AB", "SO", "WC"),
                                     exception = "-",
                                     save_folder = "./data/tidy")

# convert nested list to dataframe
corpus <- data.frame(matrix(unlist(corpus_list), nrow = length(corpus_list), byrow = TRUE),
                     stringsAsFactors = FALSE)

# set colum names
colnames(corpus) <- c("ref_num", "access_num", "DOI", "authors", "title",
                      "abstract", "keywords", "year", "journal", "WOS_categories")

# replace empty cells with NA
corpus[corpus == ""] <- NA

# ajout des colonnes "text_mixed", "search_voc", "big_taxa" et "taxa" au tableau corpus
corpus <- data.frame(corpus, text_mixed = NA, search_voc = NA, big_taxa = NA, taxa = NA)

# suppression des eventuels doublons via la colonne access_num
doublons <- which(duplicated(corpus$access_num))

if (length(doublons) != 0) {
  corpus <- corpus[-doublons, ]
}

# ajout d'un espace avant et apres chaque paragraphe contenu dans titre, mots clefs, abstract, journal et WOS categorie
corpus <- addSpace(data = corpus,
                   column = c("title", "abstract", "keywords", "journal", "WOS_categories"))

#Sauvegarde
saveRDS(corpus, paste0("./output/text/", corpus_name, ".rds"))

# # les tags correspondent a :
# corpus_list[[1]]$NOID # numero de la reference donne par la fonction filterArticles_mod()
# corpus_list[[1]]$UT # WOS Accession Number
# corpus_list[[1]]$DI # DOI
# corpus_list[[1]]$AU # Authors
# corpus_list[[1]]$TI # Document Title
# corpus_list[[1]]$AB # Abstract
# corpus_list[[1]]$DE # Author Keywords
# corpus_list[[1]]$PY # Year Published
# corpus_list[[1]]$SO # Publication Name (= nom du journal)
# corpus_list[[1]]$WC # Web of Science Categories


# ### Code pour exporter le tableau des journaux * WOS Categories et le tableau des articles avec la ponctuation
# 
# # load corpus
# corpus_list <- filterArticles_mod(files = raw_corpus, 
# type = NULL, 
# save_folder = "./data/tidy")
# 
# # convert to lowercase
# corpus_list <- toLowercase_mod(data = corpus_list, 
# tag = c('TI', 'DE', 'AB', 'SO', 'WC'), 
# save_folder = "./data/tidy")
# 
# # convert nested list to dataframe
# corpus <- data.frame(matrix(unlist(corpus_list), nrow=length(corpus_list), byrow=T), stringsAsFactors=FALSE)
# 
# # set colum names
# colnames(corpus) <- c("ref_num", "access_num", "DOI", "authors", "title", "abstract", "keywords", "year", "journal", "WOS_categories")
# 
# # replace empty cells with NA
# corpus[corpus==""] <- NA
# 
# # write table with journal * WOS_categories
# journal_table <- corpus[, c(9, 10)]
# journal_table <- journal_table %>% count(journal, WOS_categories, sort = TRUE)
# write.csv2(journal_table, file = "./output/text/journal_recap_table.csv")
# 
# # WOS categories list
# WOS_categories_list <- c()
# for (i in 1:length(journal_table$WOS_categories)) {
# if (str_detect(journal_table$WOS_categories[i], ";")) {temp <- unlist(strsplit(journal_table$WOS_categories[i], "; "))}
# else {temp <- journal_table$WOS_categories[i]}
# WOS_categories_list <- append(WOS_categories_list, temp)
# }
# 
# WOS_categories_list <- sort(unique(WOS_categories_list))
# WOS_categories_list_df <- data.frame(WOS_categories_list)
# write.csv2(data.frame(WOS_categories_list), file = "./output/text/WOS_categories_list.csv")
# 
# # # write article table
# # write.csv2(corpus[, -c(1, 2, 6)], file = "./output/text/corpus.csv")
# 
# 
