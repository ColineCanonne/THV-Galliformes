
# Bibliographic synthesis of Galliformes' demographic traits from Web of Science
#
# script_corpus_galliformes_coline.R
#
# initial author : jan.perret@cefe.cnrs.fr


#-------- Clean workspace, load packages & functions ------

rm(list = ls())
source("R/00_packages.R")

files.sources <- list.files("./R/functions", full.names = TRUE)
sapply(files.sources, source)

#-------- Load data ------

# corpus
raw_corpus <- "./data/raw/WOS_THVgall_merged.txt"

### load corpus
corpus_list <- filterArticles_mod(files = raw_corpus,
                                  type = NULL, # type = NULL permet de prendre tous les documents qu'il y a dans le fichier .txt
                                  save_folder = "./data/tidy") 

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

# Sauvegarde
saveRDS(corpus, paste0("./output/text/galliformes_corpus.rds"))

# Creation du texte dans lequel cherche WOS (sans les keywords plus)
for (i in seq_len(nrow(corpus))) {
  corpus$text_mixed[i] <- paste0(" ", corpus$title[i], " ", corpus$abstract[i], " ", corpus$keywords[i], " ")
  # Remplacement des tirets par des espaces
  corpus$text_mixed[i] <- gsub(pattern = "-", replacement = " ", x = corpus$text_mixed[i])
}

# Enregistrement du corpus initial
corpus_initial <- corpus
saveRDS(corpus_initial, file = paste0("./output/text/galliformes_corpus_5_initial.rds"))
# corpus_initial <- readRDS(file = "./output/text/galliformes_corpus_5_initial.rds")



#-------- Filter data ------
corpus <- readRDS("./output/text/galliformes_corpus_5_initial.rds")


# liste des mots cles dont il faut au moins 1 pour garder l'article
cat1 <-c("demography" , "survival" , "mortality" , "reproduction" , "hatching rate" , "biometric" , "chicks" , "breeding" , "fecundity" , "productivity" , "dynamics" , "generation time")
cat2 <- c("galliform" ,"megapodiidae" , "megapode" , "cracidae" , "chachalaca" , "guans"  , "curassow" , "numididae" , "guineafowl" , "odontophoridae" ,  "quail" , "partridge" , "phasianidae" , "junglefowl" , "pheasant" , 
          "peafowl" , "peacock" , "francolin" , "grouse" , "snowcock"   , "ptarmigan" , "bobwhite" , "scrubfowl"  ,"brushturkey" , "malleefowl" , "alectura" ,  "aepypodius" , "talegalla" , "leipoa" ,   "macrocephalon" , "eulipoa" , "megapodius" ,   "ortalis" , "penelope" , "pipile" ,  "aburria" , "chamaepetes" , "penelopina"   , "oreophasis" , "nothocrax"   , "mitu" , "pauxi" , "crax", "agelastes" , "numida" , "guttera" 
          , "acryllium" , "ptilopachus" , "dendrortyx"   , "oreortyx" , "callipepla" , "philortyx" , "colinus" , "odontophorus" , "dactylortyx" , "cyrtonyx" ,   "rhynchortyx" , "meleagris" , "bonasa" , "tetrastes" , "falcipennis"   , "tetrao" , "lyrurus" , "centrocercus" , "dendragapus" , 
          "tympanuchus" , "lagopus" , "lerwa" , "tetraophasis" , "tetraogallus"   , "alectoris" , "ammoperdix" , "francolinus" , "peliperdix"   , "scleroptila" , "dendroperdix" , "pternistis" 
          , "perdix" , "rhizothera" , "margaroperdix" , "melanoperdix"   , "coturnix" , "excalfactoria" , "anurophasis" , "perdicula" , "ophrysia" , "xenoperdix" , "arborophila" , "caloperdix"   , "haematortyx" , "rollulus" , "bambusicola" , "galloperdix" ,   "ithaginis" , "tragopan" , "pucrasia" , "lophophorus" ,
          "gallus" , "lophura" , "crossoptilon" , "catreus" , "syrmaticus"   , "phasianus" , "chrysolophus" , "polyplectron" , "rheinardia" 
          , "argusianus" , "pavo" , "afropavo" )


voc_list <- c(cat1, cat2)

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


## On selectionne les lignes ou un seul mot clef est relevé, qui veut dire que le second avait été trouvé dans les keywords
#On retire ceux non galliformes, et parmis les mots clefs galliformes on ne garde que ceux qui descendentà l'espèce 

onekeyword <- which(sapply(strsplit(corpus$search_voc, " "), length)==1)
int <- corpus[onekeyword,]
keep <- c("bonasa","chrysolophus","crax","junglefowl","lagopus","leipoa","megapodiidae","meleagris",
          "penelope","ptarmigan","pternistis","tympanuchus")
indices_keep <- NULL
for (k in 1:length(keep)) {
  indices_keep <- c(indices_keep, which(int$search_voc == keep[k]))
}
corpus <- corpus[-onekeyword[-indices_keep],]

# On regarde si bien un mot clef de chaque

is_bothcat <- rep(NA, length(corpus$search_voc))
iscat1 <- iscat2 <- NULL
for (c in 1:length(corpus$search_voc)) {
  nb_words <- sapply(strsplit(corpus$search_voc[c], " "), length)
  iscat1 <- iscat2 <- rep(NA, nb_words)
  for (w in 1:nb_words) {
    iscat1[w] <-
      length(grep(strsplit(corpus$search_voc[c], " ")[[1]][w] , cat1))
    iscat2[w] <-
      length(grep(strsplit(corpus$search_voc[c], " ")[[1]][w] , cat2))
  }
  if (sum(iscat1) > 0 &  sum(iscat2) > 0) {
    is_bothcat[c] <- TRUE
  }
}
corpus <- corpus[which(is_bothcat==TRUE),]


# Tri sur les titres de journaux
write.table(unique(corpus$"journal"),file="titre des journaux.txt")
write.table(table(corpus$"journal"),file="titre des journaux nb .txt")
remove <- read.csv2("journaux_hs.csv")
indices_remove <- NULL
for (j in 1:nrow(remove)) {
  indices_remove <-
    c(indices_remove, which(corpus$"journal" == remove$titre[j]))
  
}
corpus <- corpus[-indices_remove, ]



# Tri sur les catégories WOS FONCTIONNE PAS
write.table(unique(corpus$"WOS_categories"),file="WOS categories.txt")
remove2 <- read.csv2("categoriesWOS_hs.csv")
indices_remove_2 <- NULL
for (j in 1:nrow(remove2)) {
  indices_remove_2 <-
    c(indices_remove_2, which(corpus$WOS_categories== remove2$titre[j]))
}
corpus <- corpus[-indices_remove_2, ]




#-------- Assign_taxa ------

# Corpus avec toutes les references
raw_latin_corpus <- "./data/raw/WOS_THVgall_merged.txt"

### Load latin_corpus
latin_corpus_list <- filterArticles_mod(files = raw_latin_corpus,
                                        type = NULL,
                                        save_folder = "./data/tidy")

# Convert nested list to dataframe
latin_corpus <- data.frame(matrix(unlist(latin_corpus_list),
                                  nrow = length(latin_corpus_list),
                                  byrow = TRUE),
                           stringsAsFactors = FALSE)

# Set colum names
colnames(latin_corpus) <- c("ref_num", "access_num", "DOI", "authors",
                            "title", "abstract", "keywords", "year",
                            "journal", "WOS_categories")

# Replace empty cells with NA
latin_corpus[latin_corpus == ""] <- NA

# Filtrage du latin_corpus au meme niveau que le corpus
latin_corpus <- latin_corpus[latin_corpus$access_num %in% corpus$access_num, ]

# Ajout colonne text_mixed et suppression de la ponctuation sauf "-"
latin_corpus <- data.frame(latin_corpus, text_mixed = NA)
exception <- "-" # Pour ne pas supprimer les tirets des mots clefs
expr <- paste("([", paste("", exception, collapse = "", sep = ""),
              "])|[[:punct:]]", collapse = "", sep = "")

for (i in seq_len(nrow(latin_corpus))) {
  latin_corpus$text_mixed[i] <- paste0(gsub(expr, "\\1", latin_corpus$title[i]), " ",
                                       gsub(expr, "\\1", latin_corpus$abstract[i]), " ",
                                       gsub(expr, "\\1", latin_corpus$keywords[i]))
}

# Write article table
write.csv2(latin_corpus, file = paste0("./gnfinder/latin_corpus_galliformes_analysis.csv"))

# Initialisation tableau output (10 fois plus grand que latin_corpus)
names_tab <- data.frame(name = rep(NA, 10*nrow(latin_corpus)), matched_name = NA,
                        classification_rank = NA, classification_path = NA,
                        source_title = NA, current_name = NA,
                        match_type = NA, matched_canonical = NA,
                        superkingdom = NA, kingdom = NA,
                        phylum = NA, subphylum = NA,
                        superclass = NA, class = NA,
                        subclass = NA, infraclass = NA,
                        superorder = NA, order = NA,
                        suborder = NA, infraorder = NA,
                        superfamily = NA, family = NA,
                        subfamily = NA, tribe = NA,
                        genus = NA, species = NA,
                        infraspecies = NA, access_num = NA)

# Lancement du programme gnfinder
# on complete la hierarchie taxo entre les differentes bases de donnees

if (Sys.info()["sysname"] == "Windows") {
  print("Windows version")
  names_tab <- nameAssociation4(command = shell,
                                search_data = latin_corpus,
                                names_tab = names_tab)
} else {
  print("OSX or Linux version")
  names_tab <- nameAssociation4(command = system,
                                search_data = latin_corpus,
                                names_tab = names_tab)
}

names_tab <- taxoComplete(tab = names_tab, first_column = 10, last_column = 27)

# sauvegarde du tableau resultant

saveRDS(object = names_tab, file = paste0("./output/text/galliformes/names_tab.rds"))
saveRDS(object = names_tab, file = paste0("./output/text/galliformes/names_tab_sauv.rds"))
# names_tab <- readRDS(file = "./output/text/galliformes/names_tab.rds")


#-------- Add taxa to corpus ------

# Association des noms aux articles et suppression des articles non assignes

### Ajout des colonnes de classification aux articles selon les noms latins
corpus <- data.frame(corpus, matched_canonical = NA, superkingdom = NA,
                     kingdom = NA, phylum = NA, subphylum = NA,
                     superclass = NA, class = NA, subclass = NA,
                     infraclass = NA, superorder = NA, order = NA,
                     suborder = NA, infraorder = NA, superfamily = NA,
                     family = NA, subfamily = NA, tribe = NA,
                     genus = NA, species = NA, infraspecies = NA)


first_fill_col <- which(colnames(corpus) == "matched_canonical")
last_fill_col <- first_fill_col + 19

corpus <- filterNames(search_data = names_tab,
                      fill_data = corpus,
                      first_fill_column = first_fill_col,
                      last_fill_column = last_fill_col)

saveRDS(corpus, file = paste0("./output/text/galliformes/corpus_attributed_names.rds"))
# corpus <- readRDS("./output/text/galliformes/corpus_attributed_names.rds")

saveRDS(corpus, file = paste0("./output/text/galliformes/corpus_attributed_names.rds"))
saveRDS(corpus, file = paste0("./output/text/galliformes/corpus_final.rds"))


#-------- Filter after taxa assignation------

# On retire les colonnes qui ne vont pas nous servir
corpus <- corpus %>% 
  select( -c("big_taxa","taxa","ref_num","superkingdom","kingdom","phylum","subphylum"
             ,"superclass","infraclass","superorder","subclass","suborder","infraorder","superfamily","family","subfamily","tribe","genus","species","infraspecies"))
             
             
# On tri les lignes où seulement gallus gallus domesticus ou Coturnix japonica en matched_canonical
#Par precaution on garde ceux où on a le mot "wild"
corpus <- 
  corpus[-which(corpus$matched_canonical=="Gallus gallus domesticus")[-
                                                                        grep("wild", corpus[which(corpus$matched_canonical=="Gallus gallus domesticus"),"text_mixed"] )
  ],]
corpus <- 
  corpus[-
           which(corpus$matched_canonical=="Coturnix japonica")
         [-   grep("wild", corpus[which(corpus$matched_canonical=="Coturnix japonica"),"text_mixed"] )
         ],]


# Enregistrement du corpus 
corpus_select_taxon <- corpus
saveRDS(corpus_select_taxon, file = paste0("./output/text/corpus_select_taxon.rds"))



corpus %>%
  filter(str_detect(text_mixed,'domestic')) %>% 
  filter(!str_detect(text_mixed,'natural')) %>% 
  filter(!str_detect(text_mixed,'wild')) -> corpus_domestic

which(corpus$access_num==corpus_domestic$access_num)

corpus %>%
  filter(str_detect(access_num,corpus_domestic$access_num)) -> try

corpus %>%
  filter(access_num,corpus_domestic$access_num)-> try
subset(corpus, corpus_domestic$access_num)

corpus ->try

try <- try[
  corpus_domestic$access_num %in% corpus$access_num
  
  , ]

### essais de tri 

"domestic"&"wild"

toMatch <- c("domestic", "wild")
matches <- unique (grep(paste(toMatch,collapse="&"), 
                        corpus$text_mixed, value=TRUE))

grep(pattern = 'domestic&wild', corpus$text_mixed)


corpus %>%
  filter(str_detect(text_mixed,'domestic')) -> corpus_try_2

paste0("domestic"&"wild") %in% corpus$text_mixed

corpus_chicken <- corpus[grep("domestic", corpus$text_mixed),]
corpus_chicken <- corpus[grep(c("domestic","wild"), corpus$text_mixed),]

corpus_chicken <- corpus[grep("chicken", corpus$text_mixed),]

unique(corpus$order)

grep("Galliformes", corpus$order)

corpus_chicken <- corpus[grep("chicken", corpus$text_mixed),]

grep("coturnix", corpus$search_voc)

grep("Galliformes", corpus$order)

no_galliform <- (1:2568)[-grep("Galliformes", corpus$order)]
corpus_nogall <- corpus[no_galliform,]


