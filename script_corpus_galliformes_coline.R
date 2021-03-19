#################################################
#
# Cross-taxa population monitoring bibliometrics
#
# script_corpus_galliformes_coline.R
#
# jan.perret@cefe.cnrs.fr
#################################################


##################
# clean workspace
##################
rm(list = ls())

##################
# load packages
##################
source("R/00_packages.R")

##################
# load functions
##################
files.sources <- list.files("./R/functions", full.names = TRUE)
sapply(files.sources, source)

# creation du dossier si inexistant
if (!dir.exists(paste0("./output/text/galliformes"))) {
  dir.create(path = paste0("./output/text/galliformes"), showWarnings = TRUE,
             recursive = FALSE, mode = "0777")
 }

##################
# load data
##################

# corpus
raw_corpus <- "./data/raw/WOS_exemple_Coline.txt"

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

# # ajout d'un espace avant et apres chaque paragraphe contenu dans titre, mots clefs, abstract, journal et WOS categorie
# corpus <- addSpace(data = corpus,
#                    column = c("title", "abstract", "keywords", "journal", "WOS_categories"))
########### il y a un bug dans la fonction addSpace(), a corriger mais cette etape ne sert plus
########### a rien de toute facon car j'ajoute les espaces a la ligne 93 lors de la creation de
########### la colonne text_mixed

# Sauvegarde
saveRDS(corpus, paste0("./output/text/galliformes_corpus.rds"))


##################
# filter data
##################

# Creation du texte dans lequel cherche WOS (sans les keywords plus)
for (i in seq_len(nrow(corpus))) {
  corpus$text_mixed[i] <- paste0(" ", corpus$title[i], " ", corpus$abstract[i], " ", corpus$keywords[i], " ")
  # Remplacement des tirets par des espaces
  corpus$text_mixed[i] <- gsub(pattern = "-", replacement = " ", x = corpus$text_mixed[i])
}

# Remplissage de la colonne search_voc

# Suppression des articles qui ont "population" uniquement dans les keywords plus
corpus <- corpus[grepl("population", corpus[, which(colnames(corpus) == "text_mixed")]), ]

# liste des mots cles dont il faut au moins 1 pour garder l'article
voc_list <- c("monitor", "dynamic", "demograph", "trend")

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

# on reproduit cette etape mais pour les mots cles relatifs a la puissance
corpus$search_voc <- NA

voc_list <- c("galliformes analysis", "statistical", "galliformes") ################ la il faut que je trouve un moyen de coder que ma recherche c'etait ("galliformes analysis" OR ("statistical" AND "galliformes"))

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

# Enregistrement du corpus initial
corpus_initial <- corpus
saveRDS(corpus_initial, file = paste0("./output/text/galliformes_corpus_5_initial.rds"))
# corpus_initial <- readRDS(file = "./output/text/galliformes_corpus_5_initial.rds")


# NOTE : j'ai laisse les articles sans abstract ainsi que les articles d'avant 1991


##################
# assign_taxa
##################

# Corpus avec toutes les references
raw_latin_corpus <- "./data/raw/WOS_exemple_Coline.txt"

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


#################################################################################### on garde que 50 lignes pour l'exemple
latin_corpus <- latin_corpus[c(1:50), ]

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

# on complete la hierarchie taxo entre les differentes bases de donnees
names_tab <- taxoComplete(tab = names_tab, first_column = 10, last_column = 27)

# sauvegarde du tableau resultant
saveRDS(object = names_tab, file = paste0("./output/text/galliformes/names_tab.rds"))
# names_tab <- readRDS(file = "./output/text/galliformes/names_tab.rds")


# Ajout classification des noms sans classif gnfinder
# names_tab <- taxizeClassification(names_tab)
# saveRDS(names_tab, file = paste0("./output/text/galliformes/names_tab2.rds"))

################################# ici taxizeClassification() n'apport aucune modification au tableau, je ne me souviens plus ce que fais cette fonction !


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

### valeurs pour explorer le fonctionnement de la fonction filterNames()
# saveRDS(names_tab, file = "./output/text/galliformes/names_tab_before_filterNames")
# saveRDS(corpus, file = "./output/text/galliformes/corpus_before_filterNames")

corpus <- filterNames(search_data = names_tab,
                      fill_data = corpus,
                      first_fill_column = first_fill_col,
                      last_fill_column = last_fill_col)

saveRDS(corpus, file = paste0("./output/text/galliformes/corpus_attributed_names.rds"))
# corpus <- readRDS("./output/text/galliformes/corpus_attributed_names.rds")

#########################

# Assignation aux grands taxons
## Creation des listes de mots clefs
## En commentaire la colonne ou le(s) trouver

# Assignation des vertebres
vertebrate_list <- c("Mammalia", "Aves",
                     "Reptilia", "Lepidosauria",
                     "Crocodilia", "Rhynchocephalia",
                     "Squamata", "Testudines",
                     "Amphibia", "Caudata",
                     "Actinopterygii", "Chondrichthyes", "Elasmobranchii", # j'ai ajoute ce dernier taxon. C'est une 'subclass' mais parfois il se retrouve en 'class'
                     "Ceratodontiformes", "Lepidosireniformes")#class et order
corpus <- taxaAssociation(corpus, search_column = c("order", "class"),
                          fill_column = "big_taxa", vertebrate_list,
                          name_taxa = " vertebrate ")

# Assignation des invertebres
invertebrate_list <- c("Acanthocephala", "Annelida", "Arthropoda", "Brachiopoda",
                       "Bryozoa", "Chaetognatha", "Cnidaria", "Ctenophora",
                       "Echinodermata", "Entoprocta", "Fornicata", "Mollusca",
                       "Nematoda", "Nemertea", "Onychophora", "Platyhelminthes",
                       "Porifera", "Rotifera", "Sipuncula", "Anopla",
                       "Anthozoa", "Arachnida", "Archiacanthocephala", "Archisagittoidea",
                       "Asteroidea", "Bdelloidea", "Bivalvia", "Branchiopoda",
                       "Calcarea", "Caudofoveata", "Cephalocarida", "Cephalopoda",
                       "Cestoda", "Chilopoda", "Chromadorea", "Clitellata",
                       "Collembola", "Craniata", "Crinoidea", "Cubozoa",
                       "Cycliophora", "Demospongiae", "Diplopoda", "Diplura",
                       "Dorylaimea", "Echinoidea", "Enopla", "Enoplea",
                       "Entoprocta", "Eoacanthocephala", "Euchelicerata", "Gastropoda",
                       "Gymnolaemata", "Hexactinellida", "Hexanauplia", "Holothuroidea",
                       "Homoscleromorpha", "Hydrozoa", "Insecta", "Lingulata",
                       "Macrostomorpha", "Malacosporea", "Malacostraca", "Maxillopoda",
                       "Monogonta", "Monoplacophora", "Myxosporea", "Nuda",
                       "Ophiuroidea", "Ostracoda", "Palaeacanthocephala", "Pararotatoria",
                       "Pauropoda", "Phascolosomatidea", "Phylactolaemata", "Polyacanthocephala",
                       "Polychaeta", "Polyplacophora", "Polypodiozoa", "Protura",
                       "Pycnogonida", "Remipedia", "Rhynchonellata", "Sagittoidea",
                       "Scaphopoda", "Scyphozoa", "Sipunculidea", "Solenogastres",
                       "Somasteroidea", "Staurozoa", "Stenolaemata", "Symphyla",
                       "Tentaculata", "Trematoda", "Trepaxonemata", "Udeonycophora")#phylum et class
corpus <- taxaAssociation(corpus, search_column = c("phylum", "class"),
                          fill_column = "big_taxa", invertebrate_list,
                          name_taxa = "invertebrate")

# Assignation des plantes
plant_list <- c("Plantae", "Viridiplantae")#kingdom
corpus <- taxaAssociation(corpus, search_column = c("kingdom"),
                          fill_column = "big_taxa", plant_list,
                          name_taxa = "plant")

# Assignation des champignons
fungi_list <- c("Fungi", "Ascomycota", "Basidiomycota", "Chytridiomycota",
                "Glomeromycota", "Myxomycota", "Zygomycota")#kingdom et phylum
corpus <- taxaAssociation(corpus, search_column = c("kingdom", "phylum"), 
                          fill_column = "big_taxa", fungi_list,
                          name_taxa = "fungi")

# # Suppression des articles non assignes
# no_taxa <- which(is.na(corpus$big_taxa))
# if (length(no_taxa) != 0) {
#   corpus <- corpus[-no_taxa, ]
# }

# Assignation aux "sous-taxons" des vertebres
## Creation des listes de mots clefs
## En commentaire la colonne ou le(s) trouver

# Assignation des oiseaux
bird_list <- c("Aves")#class
corpus <- taxaAssociation(corpus, search_column = c("class"),
                          fill_column = "taxa", bird_list,
                          name_taxa = "bird")

# Assignation des mammiferes
mammal_list <- c("Mammalia")#class
corpus <- taxaAssociation(corpus, search_column = c("class"),
                          fill_column = "taxa", mammal_list, 
                          name_taxa = "mammal")

# Assignation des reptiles
reptile_list <- c("Reptilia", "Lepidosauria",
                  "Crocodilia", "Rhynchocephalia",
                  "Squamata", "Testudines")#class et order
corpus <- taxaAssociation(corpus, search_column = c("class", "order"),
                          fill_column = "taxa", reptile_list,
                          name_taxa = "reptile")

# Assignation des amphibiens
amphibian_list <- c("Amphibia", "Caudata")#class et order
corpus <- taxaAssociation(corpus, search_column = c("class", "order"),
                          fill_column = "taxa", amphibian_list, 
                          name_taxa = "amphibian")

# Assignation des poissons
fish_list <- c("Actinopterygii", "Chondrichthyes", "Elasmobranchii", # ce dernier taxon c'est une 'subclass' mais parfois il se retrouve en 'class'
               "Ceratodontiformes", "Lepidosireniformes")#class et order
corpus <- taxaAssociation(corpus, search_column = c("class", "order"), 
                          fill_column = "taxa", fish_list, 
                          name_taxa = "fish")

### association des autres groupes, pour le moment on fait tres grossierement
# invertebres
corpus <- taxaAssociation(corpus, search_column = c("phylum", "class"),
                          fill_column = "taxa", invertebrate_list,
                          name_taxa = "invertebrate")

# champignons
corpus <- taxaAssociation(corpus, search_column = c("kingdom", "phylum"), 
                          fill_column = "taxa", fungi_list,
                          name_taxa = "fungi")

# plantes
corpus <- taxaAssociation(corpus, search_column = c("kingdom"),
                          fill_column = "taxa", plant_list,
                          name_taxa = "plant")

saveRDS(corpus, file = paste0("./output/text/galliformes/corpus_final.rds"))
# corpus <- readRDS("./output/text/galliformes/corpus_final.rds")

# composition taxo du corpus
rev(sort(table(corpus$big_taxa, useNA = "always")))
rev(sort(table(corpus$taxa, useNA = "always")))
table(corpus$big_taxa, corpus$taxa, useNA = "always")

# NOTE : il y a des doubles assignations entre des taxons de vertebres donc c'est normal que sur la
# figure du nombre d'articles par taxon de vertebre ca somme a plus que le le nombre de vertebres 
# sur la figure du nombre d'articles par "big_taxa"


########################
# Figures corpus final #
########################

corpus_final <- corpus

# Liste contenant tous les noms de "grands" taxons
name_taxa_list <- c("fungi", "invertebrate", "plant", " vertebrate") # attention il y a un espace avant " vertebrate" pour qu'il soit differencie de "invertebrate"

# Liste contenant le nombre d'especes par "grands" taxon
number_species_list <- c(30000, 1232384, 288468, 61259)

# pour avoir les memes couleurs sur chaque figure
colours = c("vertebrate" = "darkorange1", "invertebrate" = "cyan3",
            "plant" = "seagreen3", "fungi" = "darkorchid2")

## Au total
nb_taxa_list <- NULL
for (i in name_taxa_list) {
  nb_taxa_list <- c(nb_taxa_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "big_taxa")])))
} # Liste contenant le nombre total d'articles par grand taxon
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)

# Nombres d'articles publies par taxon au total
nb_taxon_tab <- data.frame(taxa = name_taxa_list, nb = nb_taxa_list)

articles_par_taxon <- ggplot(nb_taxon_tab, aes(x = nb, y = reorder(taxa, nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
           fill = c("darkorchid2", "cyan3", "seagreen3", "darkorange1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa")

# Save figures in PDF
pdf(file = "./output/plots/galliformes_articles_par_grand_taxon.pdf", width = 11.69, height = 8.27)
articles_par_taxon
dev.off()


# Au cours du temps
nb_taxonPyear_tab <- data.frame(year = 1991:2019, vertebrate = 0, invertebrate = 0, plant = 0, fungi = 0)

for (i in seq_len(nrow(corpus_final))) { # loop through rows
  
  if (!is.na(corpus_final$big_taxa[i])) { # 
    
    for (j in name_taxa_list) { # 
      
      if (grepl(j, corpus_final[i, which(colnames(corpus_final) == "big_taxa")])) {
        nb_taxonPyear_tab[which(nb_taxonPyear_tab$year == corpus_final$year[i]),
                          which(colnames(nb_taxonPyear_tab) == gsub(" ", "", j, fixed = TRUE))] <- # ici le gsub() sert a supprimer l'espace qui se trouve devant " vertebrate"
          nb_taxonPyear_tab[which(nb_taxonPyear_tab$year == corpus_final$year[i]),
                            which(colnames(nb_taxonPyear_tab) == gsub(" ", "", j, fixed = TRUE))] + 1
      }
    }
  }
}

# Tableau contenant le nombre d'articles par annee et par grand taxon
nb_taxonPyear_tab2 <- gather(
  data = nb_taxonPyear_tab,
  key = TYPE,
  value = VAL,
  vertebrate, invertebrate, plant, fungi)

taxon_par_an <- ggplot(nb_taxonPyear_tab2, aes(x = year, y = VAL, color = TYPE)) +
                      geom_line(size = 2) +
                      labs(y = "Number of articles", x = "Years") +
                      guides(col=guide_legend("Taxa")) +
                      scale_color_manual(values = colours)
# Graphique regroupant le nombre d'articles publies chaque annee pour chaque grand taxon

# Save figures in PDF
# pdf(file = "./output/plots/nb_grands_taxons_par_an.pdf", width = 11.69, height = 8.27)
# taxon_par_an
# dev.off()



# cumule au cours du temps
cumule_taxonPyear_tab <- nb_taxonPyear_tab

for (j in 2:ncol(nb_taxonPyear_tab)) {
  
  for (i in 2:nrow(nb_taxonPyear_tab)) {
    cumule_taxonPyear_tab[i, j] <- cumule_taxonPyear_tab[i-1, j] + cumule_taxonPyear_tab[i, j]
  }
}

# Tableau contenant le nombre d'articles cumule par annee et par grand taxon
cumule_taxonPyear_tab2 <- gather(
  data = cumule_taxonPyear_tab,
  key = TYPE,
  value = VAL,
  vertebrate, invertebrate, plant, fungi)

taxon_cumule <- ggplot(cumule_taxonPyear_tab2, aes(x = year, y = VAL, color=TYPE)) +
  geom_line(size=2) +
  labs(y = "Cumulated number of articles", x = "Years") +
  directlabels::geom_dl(aes(label = TYPE),
                        method = list(dl.trans(x = x + 0.2), "last.points", cex = 1)) +
  coord_cartesian(clip = 'off') +
  theme(legend.position = "none", plot.margin = margin(0.3, 0.3, 0.3, 0.3, "cm"))+
  expand_limits(x = c(2025)) + # Pour elargir les marges 
  scale_color_manual(values = colours)
# Graphique regroupant le nombre d'articles publies chaque annee pour chaque grand taxon

# Save figures in PDF
pdf(file = "./output/plots/galliformes_grand_taxon_cumule.pdf", width = 11.69, height = 8.27)
taxon_cumule
dev.off()


## Repartition des articles au sein des vertebres
# Liste des sous-taxons de vertebres
name_vertebrate_list <- c("amphibian", "bird", "fish", "mammal", "reptile")

# Liste du nombre d'especes par sous-taxon de vertebres
number_vertebrate_species_list <- c(6347, 9990, 30700, 5488, 8734)

## Au total

nb_vertebrate_list <- NULL
for (i in name_vertebrate_list) {
  nb_vertebrate_list <- c(nb_vertebrate_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "taxa")])))
} # Liste contenant le nombre total d'articles par sous-taxon de vertebres
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)


# Nombres d'articles publies par taxon au total
nb_vertebrate_taxon_tab <- data.frame(taxa = name_vertebrate_list, nb = nb_vertebrate_list)

articles_vertebrate_taxa <- ggplot(nb_vertebrate_taxon_tab, aes(x = nb, y = reorder(taxa,nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
           fill = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa of vertebrates")

# Save figures in PDF
pdf(file = "./output/plots/galliformes_articles_par_taxon_vertebre.pdf", width = 11.69, height = 8.27)
articles_vertebrate_taxa
dev.off()


#### KRONA

# on supprime les redondances de la hierarchie taxo

test_tab <- names_tab[c(1:100),]
View(test_tab)
View(krona_names_tab)

krona_names_tab <- filterNames_Krona(fill_data = names_tab, first_fill_column = 9, last_fill_column = 27)

# calcul du nombre d'articles par ligne POUR krona_names_tab
toto <- as.list(strsplit(krona_names_tab$access_num, ","))
n_article <- lengths(toto) # la fonction lengths avec un "s" donne la longueur de tous les elements d'une liste !!
krona_names_tab <- cbind(krona_names_tab, n_article = n_article)

# # calcul du nombre d'articles par ligne POUR names_tab ######### -> sans supression des redondances !
# toto <- as.list(strsplit(names_tab$access_num, ","))
# n_article <- lengths(toto) # la fonction lengths avec un "s" donne la longueur de tous les elements d'une liste !!
# names_tab <- cbind(names_tab, n_article = n_article)

# on garde uniquement les colonnes interessantes
keep <- c("kingdom", "phylum", "class", "order", "family", "genus", "species", "n_article")
krona_tab <- subset(krona_names_tab, select = c(kingdom, phylum, class, order, family, genus, species, n_article) )

# on drop les lignes avec un NA dans la premier colonne (c'est des bacteries)
krona_tab <- subset(krona_tab, !is.na(krona_tab$kingdom))

# on remplace les NA par des valeurs vides
krona_tab[is.na(krona_tab)] <- ""

# on ajoute une ligne avec le nombre d'articles non assignes

# table(corpus_initial$access_num %in% corpus$access_num)

# nombre d'articles dans le corpus initial - articles avec au moins une assignation taxo
n_not_assigned <- nrow(corpus_initial) - nrow(corpus)
not_assigned_articles <- c("Not assigned", "", "", "", "", "", "", n_not_assigned)
krona_tab <- rbind(not_assigned_articles, krona_tab)

############################################################################
# la il faudrait ajouter une fonction qui verifie qu'il n'y a pas de NA en amont de niveaux 
# taxo connus car je crois que ca fait buguer le template Excel 

write.csv2(krona_tab, file = "./output/text/galliformes_corpus_names_tab_krona.csv")


### tableaux recap du corpus pour aurelien
latin_corpus <- read.csv2(file = "./gnfinder/latin_corpus_galliformes_analysis.csv")
# View(latin_corpus)

latin_corpus <- subset(latin_corpus, select = -c(X, text_mixed) ) # on supprime les colonnes qui ne servent a rien
corpus_join <- corpus[, c(2, 13:34)]
# View(corpus_join)

corpus_export <- left_join(latin_corpus, corpus_join, by = "access_num")
View(corpus_export)

write.csv2(corpus_export, file = "./output/text/galliformes_corpus_pour_export.csv")






#### test pour verifier qu'aucune valeur de text_mixed ne depasse 8191 caracteres (la limite de ligne de commande Windows) 
hist(nchar(corpus_initial$text_mixed))
# -> c'est bon c'est pas le cas

