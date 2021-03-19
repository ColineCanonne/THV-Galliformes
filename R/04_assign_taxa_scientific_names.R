#################################################
#
# Cross-taxa population monitoring bibliometrics
#
# 04_assign_taxa_scientific_names.R
#
# jan.perret@cefe.cnrs.fr
#################################################


# Corpus avec toutes les references
raw_latin_corpus <- paste0("./data/raw/", corpus_name, ".txt")

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

# Suppression des doublons
doublons <- which(duplicated(latin_corpus$access_num))

if (length(doublons) != 0) {
  latin_corpus <- latin_corpus[-doublons, ]
}

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
write.csv2(latin_corpus, file = paste0("./gnfinder/latin_corpus_", folder, ".csv"))

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
  names_tab <- nameAssociation2(command = shell,
                                search_data = latin_corpus,
                                names_tab = names_tab)
  
} else {
  
  print("OSX or Linux version")
  names_tab <- nameAssociation2(command = system,
                                search_data = latin_corpus,
                                names_tab = names_tab)
  
}

saveRDS(object = names_tab, file = paste0("./output/text/", folder, "/names_tab.rds"))

# Ajout classification des noms sans classif gnfinder
names_tab <- taxizeClassification(names_tab)
saveRDS(names_tab, file = paste0("./output/text/", folder, "/names_tab2.rds"))

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

saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_attributed_names.rds"))

# Completion du tableau recap
recap_tab[6, column] <- round((nrow(corpus)/recap_tab[5, column]), 2)


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
                     "Actinopterygii", "Chondrichthyes",
                     "Ceratodontiformes", "Lepidosireniformes")#class et order
corpus <- taxaAssociation(corpus, search_column = c("order", "class"),
                          fill_column = "big_taxa", vertebrate_list,
                          name_taxa = " vertebrate ")

# Assignation des invertebres
invertebrate_list <- c("Acanthocephala", "Annelida",
                       "Arthropoda", "Brachiopoda",
                       "Bryozoa", "Chaetognatha",
                       "Cnidaria", "Ctenophora",
                       "Echinodermata", "Entoprocta",
                       "Fornicata", "Mollusca",
                       "Nematoda", "Nemertea",
                       "Onychophora", "Platyhelminthes",
                       "Porifera", "Rotifera",
                       "Sipuncula", "Anopla",
                       "Anthozoa", "Arachnida",
                       "Archiacanthocephala", "Archisagittoidea",
                       "Asteroidea", "Bdelloidea",
                       "Bivalvia", "Branchiopoda",
                       "Calcarea", "Caudofoveata",
                       "Cephalocarida", "Cephalopoda",
                       "Cestoda", "Chilopoda",
                       "Chromadorea", "Clitellata",
                       "Collembola", "Craniata",
                       "Crinoidea", "Cubozoa",
                       "Cycliophora", "Demospongiae",
                       "Diplopoda", "Diplura",
                       "Dorylaimea", "Echinoidea",
                       "Enopla", "Enoplea",
                       "Entoprocta", "Eoacanthocephala",
                       "Euchelicerata", "Gastropoda",
                       "Gymnolaemata", "Hexactinellida",
                       "Hexanauplia", "Holothuroidea",
                       "Homoscleromorpha", "Hydrozoa",
                       "Insecta", "Lingulata",
                       "Macrostomorpha", "Malacosporea",
                       "Malacostraca", "Maxillopoda",
                       "Monogonta", "Monoplacophora",
                       "Myxosporea", "Nuda",
                       "Ophiuroidea", "Ostracoda",
                       "Palaeacanthocephala", "Pararotatoria",
                       "Pauropoda", "Phascolosomatidea",
                       "Phylactolaemata", "Polyacanthocephala",
                       "Polychaeta", "Polyplacophora",
                       "Polypodiozoa", "Protura",
                       "Pycnogonida", "Remipedia",
                       "Rhynchonellata", "Sagittoidea",
                       "Scaphopoda", "Scyphozoa",
                       "Sipunculidea", "Solenogastres",
                       "Somasteroidea", "Staurozoa",
                       "Stenolaemata", "Symphyla",
                       "Tentaculata", "Trematoda",
                       "Trepaxonemata", "Udeonycophora")#phylum et class
corpus <- taxaAssociation(corpus, search_column = c("phylum", "class"),
                          fill_column = "big_taxa", invertebrate_list,
                          name_taxa = "invertebrate")

# Assignation des plantes
plant_list <- c("Plantae", "Viridiplantae")#kingdom
# "Andreaeobryopsida", "Andreaeopsida",
# "Anthocerotopsida", "Bryopsida",
# "Cycadopsida", "Ginkgoopsida",
# "Gnetopsida", "Haplomitriopsida",
# "Jungermanniopsida", "Leiosporocerotopsida",
# "Liliopsida", "Lycopodiopsida",
# "Magnoliopsida", "Marchantiopsida",
# "Oedipodiopsida", "Pinopsida",
# "Polypodiopsida", "Polytrichopsida",
# "Sphagnopsida", "Takakiopsida",
# "Tetraphidopsida", "Apiales",
# "Asterales", "Caryophyllales",
# "Fabales", "Fagales",
# "Lamiales", "Malvales",
# "Myrtales", "Principes",
# "Ranunculales", "Sapindales",
## class et order
corpus <- taxaAssociation(corpus, search_column = c("kingdom"),
                          fill_column = "big_taxa", plant_list,
                          name_taxa = "plant")

# Assignation des champignons
fungi_list <- c("Fungi", "Ascomycota", "Basidiomycota", "Chytridiomycota",
                "Glomeromycota", "Myxomycota", "Zygomycota")#kingdom et phylum
corpus <- taxaAssociation(corpus, search_column = c("kingdom", "phylum"), 
                          fill_column = "big_taxa", fungi_list,
                          name_taxa = "fungi")

# Suppression des articles non assignes
no_taxa <- which(is.na(corpus$big_taxa))
if (length(no_taxa) != 0) {
  corpus <- corpus[-no_taxa, ]
}

saveRDS(corpus, file = paste0("./output/text/", folder, "/corpus_final.rds"))

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
fish_list <- c("Actinopterygii", "Chondrichthyes",
               "Ceratodontiformes", "Lepidosireniformes")#class et order
corpus <- taxaAssociation(corpus, search_column = c("class", "order"), 
                          fill_column = "taxa", fish_list, 
                          name_taxa = "fish")

#  Assignation aux "sous-taxons" des invertebres
## Creation des listes de mots clefs
## En commentaire la colonne ou le(s) trouver

# Assignation des crustaces
crustacean_list <- c("Branchiopoda", "Cephalocarida", "Malacostraca",
                     "Maxillopoda", "Ostracoda", "Remipedia")#class
corpus <- taxaAssociation(corpus, search_column = c("class"), 
                          fill_column = "taxa", crustacean_list, 
                          name_taxa = "crustacean")

# Assignation des mollusques
mollusca_list <- c("Mollusca")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", mollusca_list, 
                          name_taxa = "mollusca")

# Assignation des odonates
odonata_list <- c("Odonata")#order
corpus <- taxaAssociation(corpus, search_column = c("order"), 
                          fill_column = "taxa", odonata_list, 
                          name_taxa = "odonata")

# Assignation des lepidopteres
lepidoptera_list <- c("Lepidoptera")#order
corpus <- taxaAssociation(corpus, search_column = c("order"), 
                          fill_column = "taxa", lepidoptera_list, 
                          name_taxa = "lepidoptera")

# Assignation des cheliceres
chelicerata_list <- c("Arachnida", "Euchelicerata", "Pycnogonida")#class
corpus <- taxaAssociation(corpus, search_column = c("class"), 
                          fill_column = "taxa", chelicerata_list, 
                          name_taxa = "chelicerata")

# Assignation des cnidaires
cnidaria_list <- c("Cnidaria")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", cnidaria_list, 
                          name_taxa = "cnidaria")

# Assignation des coleopteres
coleoptera_list <- c("Coleoptera")#order
corpus <- taxaAssociation(corpus, search_column = c("order"), 
                          fill_column = "taxa", coleoptera_list, 
                          name_taxa = "coleoptera")

# Assignation des hymenopteres
hymenoptera_list <- c("Hymenoptera")#order
corpus <- taxaAssociation(corpus, search_column = c("order"), 
                          fill_column = "taxa", hymenoptera_list, 
                          name_taxa = "hymenoptera")

# Assignation des autres invertebres
other_invertebrates_list <- c("Acanthocephala", "Annelida",
                              "Brachiopoda", "Bryozoa",
                              "Chaetognatha", "Ctenophora",
                              "Echinodermata", "Entoprocta",
                              "Fornicata", "Nematoda",
                              "Nemertea", "Onychophora",
                              "Platyhelminthes", "Porifera",
                              "Rotifera", "Sipuncula",
                              "Chilopoda", "Collembola",
                              "Diplopoda", "Diplura",
                              "Pauropoda", "Protura",
                              "Symphyla", "Archaeognatha",
                              "Zygentoma", "Diptera",
                              "Mecoptera", "Siphonaptera",
                              "Strepsiptera", "Trichoptera",
                              "Megaloptera", "Neuroptera",
                              "Raphidioptera", "Hemiptera",
                              "Psocodea", "Thysanoptera",
                              "Blattodea", "Dermaptera",
                              "Embioptera", "Grylloblattodea",
                              "Mantodea", "Mantophasmatodea",
                              "Orthoptera", "Phasmida",
                              "Plecoptera", "Zoraptera",
                              "Ephemeroptera")#phylum, class, order
corpus <- taxaAssociation(corpus, search_column = c("phylum", "class", "order"), 
                          fill_column = "taxa", other_invertebrates_list, 
                          name_taxa = "other_invert")


# Assignation aux "sous-taxons" des plantes
## Creation des listes de mots clefs
## En commentaire la colonne ou le(s) trouver

# Assignation des algues
algae_list <- c("Glaucophyta", "Charophyta", "Chlorophyta",
                "Rhodophyta", "Ochrophyta", "Heterokontophyta")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", algae_list, 
                          name_taxa = "algae")

# Assignation des angiospermes
angiosperm_list <- c("Liliopsida", "Magnoliopsida")#class
corpus <- taxaAssociation(corpus, search_column = c("class"), 
                          fill_column = "taxa", angiosperm_list, 
                          name_taxa = "angiosperm")

# Assignation des gymnospermes
gymnosperm_list <- c("Cycadopsida", "Ginkgoopsida",
                      "Gnetopsida", "Pinopsida")#class
corpus <- taxaAssociation(corpus, search_column = c("class"), 
                          fill_column = "taxa", gymnosperm_list, 
                          name_taxa = "gymnosperm")

# Assignation des pteridophytes
pteridophyte_list <- c("Lycopodiopsida", "Polypodiopsida")#class
corpus <- taxaAssociation(corpus, search_column = c("class"), 
                          fill_column = "taxa", pteridophyte_list, 
                          name_taxa = "pteridophyte")

# Assignation des bryophytes sensus lato
bryophyte_sl_list <- c("Anthocerotophyta", "Bryophyta",
                       "Marchantiophyta", "Hepaticophyta",
                       "Hepatophyta")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", bryophyte_sl_list, 
                          name_taxa = "bryophyte")

# Assignation aux "sous-taxons" des champignons
## Creation des listes de mots clefs
## En commentaire la colonne ou le(s) trouver

# Assignation des ascomycetes
ascomycete_list <- c("Ascomycota")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", ascomycete_list, 
                          name_taxa = "ascomycete")

# Assignation des basidiomycetes
basidiomycete_list <- c("Basidiomycota")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", basidiomycete_list, 
                          name_taxa = "basidiomycete")

# Assignation des autres champignons
other_fungi_list <- c("Chytridiomycota", "Glomeromycota",
                      "Myxomycota", "Zygomycota")#phylum
corpus <- taxaAssociation(corpus, search_column = c("phylum"), 
                          fill_column = "taxa", other_fungi_list, 
                          name_taxa = "other_fungi")