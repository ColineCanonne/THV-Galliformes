#################################################
#
# Cross-taxa population monitoring bibliometrics
#
# 05_corpus_analysis
#
# jan.perret@cefe.cnrs.fr
#################################################

#########################
# Telechargement corpus #
#########################

corpus_initial <- readRDS("./output/text/monitoring/corpus_initial.rds")
corpus_2 <- readRDS("./output/text/monitoring/corpus_2.rds")
corpus_4 <- readRDS("./output/text/monitoring/corpus_4.rds")
corpus_final <- readRDS("./output/text/monitoring/corpus_final.rds")


##########################
# Figures corpus initial #
##########################

### Presence/abscence de resume

abstract_tab <- data.frame(year = 1922:2019, with_abstract = 0, no_abstract = 0)
# Debute a 1922 car pas d'articles avant

for (i in seq_len(nrow(corpus_initial))) {
  ligne <- which(abstract_tab$year == corpus_initial$year[i])
  
  if (length(ligne) != 0) {
    
    if (corpus_initial$abstract[i] == "  NA  ") {
      # Deux espaces de chaque cote sinon cela ne fonctionne pas
      abstract_tab$no_abstract[ligne] <- abstract_tab$no_abstract[ligne] + 1
    }
    
    else {
      abstract_tab$with_abstract[ligne] <- abstract_tab$with_abstract[ligne] + 1
    }
  }
}

# Tableau contenant le nombre d'articles par annee avec ou sans abstract
abstract_tab2 <- gather(
  data = abstract_tab,
  key = TYPE,
  value = VAL,
  with_abstract, no_abstract) 

# Figure
with_without_abstract <- ggplot(abstract_tab2, aes(x = year, y = VAL, fill = TYPE)) +
  geom_bar(stat = "identity") +
  labs(y = "Number of articles",
       x = "Publication year") +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels = c("Without abstract", "With abstract"))

# Save figure in PDF
pdf(file = "./output/plots/with_without_abstract.pdf", width = 11.69, height = 8.27)
with_without_abstract
dev.off()

####################
# Figures corpus 2 #
####################

# Proportion d'articles sans mots clefs avant suppression des categories
keywords_tab <- data.frame(year = 1991:2019, with_keywords = 0, no_keywords = 0)
for (i in seq_len(nrow(corpus_2))) {
  ligne <- which(keywords_tab$year == corpus_2$year[i])
  if (length(ligne) != 0) {
    if (corpus_2$keywords[i] == "  NA  ") {
      # deux espaces de chaque cote sinon cela ne fonctionne pas
      keywords_tab$no_keywords[ligne] <- keywords_tab$no_keywords[ligne] + 1
    } else {
      keywords_tab$with_keywords[ligne] <- keywords_tab$with_keywords[ligne] + 1
    }
  }
}

# Sauvegarde dans un csv
write.csv2(keywords_tab, file = "./data/raw/avec_sans_keyword.csv")

# Tableau contenant le nombre d'articles par annee avec ou sans keywords
keywords_tab2 <- gather(
  data = keywords_tab,
  key = TYPE,
  value = VAL,
  with_keywords, no_keywords) # Nouveau tableau permettant le regroupement des variables

with_without_keyword2 <- ggplot(keywords_tab2, aes(x = year, y = VAL, fill = TYPE)) +
  geom_bar(stat = "identity") +
  labs(y = "Nombre d'articles",
       x = "Années de publication") +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels = c("Sans mot clef", "Avec mot clef"))

# Save figures in PDF
pdf(file = "./output/plots/with_without_keyword.pdf", width = 11.69, height = 8.27)
with_without_keyword2
dev.off()

####################
# Figures corpus 4 #
####################

### Meme figure apres filtrage

# Quelle proportion d'articles sans mots clefs ? Sur le corpus avant taxons
keywords_tab3 <- data.frame(year = 1991:2019, with_keywords = 0, no_keywords = 0)
for (i in seq_len(nrow(corpus_4))) {
  ligne <- which(keywords_tab3$year == corpus_4$year[i])
  if (length(ligne) != 0) {
    if (corpus_4$keywords[i] == "  NA  ") { # deux espaces de chaque cote sinon cela ne fonctionne pas
      keywords_tab3$no_keywords[ligne] <- keywords_tab3$no_keywords[ligne] + 1
    } else {
      keywords_tab3$with_keywords[ligne] <- keywords_tab3$with_keywords[ligne] + 1
    }
  }
}
# Tableau contenant le nombre d'articles par annee avec ou sans keywords
keywords_tab4 <- gather(
  data = keywords_tab3,
  key = TYPE,
  value = VAL,
  with_keywords, no_keywords) # Nouveau tableau permettant le regroupement des variables

with_without_keyword3 <- ggplot(keywords_tab4, aes(x = year, y = VAL, fill = TYPE)) +
  geom_bar(stat = "identity") +
  labs(y = "Nombre d'articles",
       x = "Années de publication") +
  theme(legend.title = element_blank()) +
  scale_fill_discrete(labels = c("Sans mot clef", "Avec mot clef"))

# Save figures in PDF
pdf(file = "./output/plots/with_without_keyword_av_taxon.pdf", width = 11.69, height = 8.27)
with_without_keyword3
dev.off()

########################
# Figures corpus final #
########################

### Quels sont les taxons qui ont ete les plus etudies ?

# Liste contenant tous les noms de "grands" taxons
name_taxa_list <- c("fungi", "invertebrate", "plant", "vertebrate")

# Liste contenant le nombre d'especes par "grands" taxon
number_species_list <- c(30000, 1232384, 288468, 61259)


## Au total

nb_taxa_list <- NULL
for (i in name_taxa_list) {
  nb_taxa_list <- c(nb_taxa_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "big_taxa")])))
} # Liste contenant le nombre total d'articles par grand taxon
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)


# Nombres d'articles publies par taxon au total
nb_taxon_tab <- data.frame(taxa = name_taxa_list, nb = nb_taxa_list)

articles_par_taxon <- ggplot(nb_taxon_tab, aes(x = nb, y = reorder(taxa,nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
           fill = c("darkorchid2", "cyan3", "seagreen3", "darkorange1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_grand_taxon.pdf", width = 11.69, height = 8.27)
articles_par_taxon
dev.off()


# Nombre d'articles publies par grand taxon au total/nombre d'especes estime
nb_taxon_species_tab <- data.frame(taxa = name_taxa_list, nb = round(nb_taxa_list/number_species_list, 3))

articles_par_taxon <- ggplot(nb_taxon_species_tab, aes(x = nb, y = reorder(taxa,nb))) +
  #reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
           fill = c("darkorchid2", "cyan3", "seagreen3", "darkorange1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("darkorchid2", "cyan3", "seagreen3", "darkorange1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Proportion of articles depending on species number", y = "Taxa")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_grand_taxon_par_especes.pdf", width = 11.69, height = 8.27)
articles_par_taxon
dev.off()


# Au cours du temps
nb_taxonPyear_tab <- data.frame(year = 1991:2019, vertebrate = 0, invertebrate = 0, plant = 0, fungi = 0)

for (i in seq_len(nrow(corpus_final))) {
  
  if (!is.na(corpus_final$big_taxa[i])) {
    
    for (j in name_taxa_list) {
      
      if (grepl(j, corpus_final[i, which(colnames(corpus_final) == "big_taxa")])) {
        nb_taxonPyear_tab[which(nb_taxonPyear_tab$year == corpus_final$year[i]),
                          which(colnames(nb_taxonPyear_tab) == j)] <- 
          nb_taxonPyear_tab[which(nb_taxonPyear_tab$year == corpus_final$year[i]),
                            which(colnames(nb_taxonPyear_tab) == j)] + 1
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

taxon_par_an <- ggplot(nb_taxonPyear_tab2, aes(x = year, y = VAL, color=TYPE)) +
  geom_line(size=2)+
  labs(y = "Number of articles", x = "Years")+
  guides(col=guide_legend("Taxa"))
# Graphique regroupant le nombre d'articles publies chaque annee pour chaque grand taxon

# Save figures in PDF
pdf(file = "./output/plots/nb_grands_taxons_par_an.pdf", width = 11.69, height = 8.27)
taxon_par_an
dev.off()


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
  expand_limits(x = c(2025)) # Pour elargir les marges 
# Graphique regroupant le nombre d'articles publies chaque annee pour chaque grand taxon

# Save figures in PDF
pdf(file = "./output/plots/grand_taxon_cumule.pdf", width = 11.69, height = 8.27)
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
pdf(file = "./output/plots/articles_par_taxon_vertebre.pdf", width = 11.69, height = 8.27)
articles_vertebrate_taxa
dev.off()

# Nombre d'articles publies par sous_taxon de vertebres au total/nombre d'especes estime
nb_vertebrate_species_tab <- data.frame(taxa = name_vertebrate_list, nb = round(nb_vertebrate_list/number_vertebrate_species_list, 3))

articles_vertebrate_taxon <- ggplot(nb_vertebrate_species_tab, aes(x = nb, y = reorder(taxa,nb))) +
  #reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
           fill = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Proportion of articles depending on species number", y = "Taxa")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_taxon_vertebre_par_especes.pdf", width = 11.69, height = 8.27)
articles_vertebrate_taxon
dev.off()


## Repartition des articles au sein des invertebres

# Liste des sous-taxons de invertebres
name_invertebrate_list <- c("hymenoptera", "coleoptera", "cnidaria", "chelicerata",
                            "lepidoptera" , "odonata", "mollusca", "crustacean",
                            "other_invert")


## Au total

nb_invertebrate_list <- NULL
for (i in name_invertebrate_list) {
  nb_invertebrate_list <- c(nb_invertebrate_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "taxa")])))
} # Liste contenant le nombre total d'articles par sous-taxon d'invertebres
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)


# Nombres d'articles publies par taxon au total
nb_invertebrate_taxon_tab <- data.frame(taxa = name_invertebrate_list, nb = nb_invertebrate_list)

articles_invertebrate_taxa <- ggplot(nb_invertebrate_taxon_tab, aes(x = nb, y = reorder(taxa,nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
           fill = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa of invertebrates")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_taxon_invertebre.pdf", width = 11.69, height = 8.27)
articles_invertebrate_taxa
dev.off()


## Repartition des articles au sein des plantes

# Liste des sous-taxons de plantes
name_plant_list <- c("bryophyte", "pteridophyte", "gymnosperm", "angiosperm", "algae")


## Au total

nb_plant_list <- NULL
for (i in name_plant_list) {
  nb_plant_list <- c(nb_plant_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "taxa")])))
} # Liste contenant le nombre total d'articles par sous-taxon de plantes
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)


# Nombres d'articles publies par sous-taxon de plantes au total
nb_plant_taxon_tab <- data.frame(taxa = name_plant_list, nb = nb_plant_list)

articles_plant_taxa <- ggplot(nb_plant_taxon_tab, aes(x = nb, y = reorder(taxa,nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
           fill = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("red3", "orange3", "olivedrab4", "dodgerblue", "maroon1"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa of plants")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_taxon_plante.pdf", width = 11.69, height = 8.27)
articles_plant_taxa
dev.off()


## Repartition des articles au sein des champignons

# Liste des sous-taxons de champignons
name_fungi_list <- c("ascomycete", "basidiomycete", "other_fungi")


## Au total

nb_fungie_list <- NULL
for (i in name_fungi_list) {
  nb_fungi_list <- c(nb_fungi_list, sum(grepl(i, corpus_final[, which(colnames(corpus_final) == "taxa")])))
} # Liste contenant le nombre total d'articles par sous-taxon de champignons
# Si un article est associe a deux taxons il sera compte deux fois (une pour chaque)


# Nombres d'articles publies par sous-taxon de champignons au total
nb_fungi_taxon_tab <- data.frame(taxa = name_fungi_list, nb = nb_fungi_list)

articles_fungi_taxa <- ggplot(nb_fungi_taxon_tab, aes(x = nb, y = reorder(taxa,nb))) +
  # reorder classe les taxons selon le nb d'articles
  geom_bar(stat = "identity",
           color = c("red3", "orange3", "olivedrab4"),
           fill = c("red3", "orange3", "olivedrab4")) +
  geom_text(aes(label = nb), vjust = 0.2, hjust = -0.1,
            color = c("red3", "orange3", "olivedrab4"),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Taxa of fungi")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_taxon_champignon.pdf", width = 11.69, height = 8.27)
articles_fungi_taxa
dev.off()

####################
# Vieilles figures #
####################


### Proportion articles sans keyword apres filtrage

No_keyword <- length(which(corpus_4$keywords == "  NA  "))
With_keyword <- length(which(corpus_4$keywords != "  NA  "))

pie(c(No_keyword, With_keyword), labels = c("Sans mot clef","Avec mot clef"), col = c("salmon", "darkturquoise"))


### Profile des journaux generaux vs ecologie

# AFC

profile_journaux_tab <- read.csv("./data/raw/profiles_journaux_principaux.csv", sep = ";", fill = T)

for (i in seq_len(nrow(profile_journaux_tab))) {
  
  journal_name <- profile_journaux_tab$journal[i]
  sous_tab <- corpus_final[which(as.character(corpus_final$journal) == paste0("  ", journal_name, "  ")), ]
  
  for (taxon in colnames(profile_journaux_tab)[2:9]) {
    
    column_index <- which(colnames(profile_journaux_tab) == taxon)
    profile_journaux_tab[i,column_index] <- sum(grepl(taxon, sous_tab$taxa))
  }
}

saveRDS(profile_journaux_tab, file = "./output/text/profile_journaux_tab.rds")

# Les premiers articles
premiers_journ_tab <- data.frame(table(sans_nom_latin$journal))
colnames(premiers_journ_tab) <- c("journal", "nombre_article")
premiers_journ_tab <- premiers_journ_tab[order(-premiers_journ_tab$nombre_article),]
premiers_journ_tab <- premiers_journ_tab[1:15, ]
premiers_journ_tab$journal <- as.character(premiers_journ_tab$journal)
premiers_journ_tab$journal[which(grepl("proceedings of the national", premiers_journ_tab$journal))] <- "  pnas  "

articles_par_journal <- ggplot(premiers_journ_tab, aes(x = nombre_article, y = reorder(journal, nombre_article))) +
  geom_bar(stat = "identity",
           color = c("blue", "forestgreen", rep("blue", 2), rep("forestgreen", 3), "blue", "forestgreen", "blue", rep("forestgreen", 5)),
           fill = c("blue", "forestgreen", rep("blue", 2), rep("forestgreen", 3), "blue", "forestgreen", "blue", rep("forestgreen", 5))) +
  geom_text(aes(label = nombre_article), vjust = 0.2,
            hjust = -0.1, color = c("blue", "forestgreen", rep("blue", 2), rep("forestgreen", 3), "blue", "forestgreen", "blue", rep("forestgreen", 5)),
            position = position_dodge(0.9), size = 3.5) +
  labs(x = "Number of articles", y = "Journal")

# Save figures in PDF
pdf(file = "./output/plots/articles_par_journal.pdf", width = 11.69, height = 8.27)
articles_par_journal
dev.off()



### Venn diagram des keywords par taxons

# 1 monitor, 2 dynamic, 3 demograph, 4 trend, 5 viability analysis
# Attention cet ordre est important
list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")


## Grands taxons

# Plant
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "plant", column_name = "big_taxa")

# Invertebrate
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "invertebrate", column_name = "big_taxa")

# Fungi
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "fungi", column_name = "big_taxa")

# Vertebrate
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "vertebrate", column_name = "big_taxa")


## Sous-taxons des vertebres

#Bird
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "bird", column_name = "taxa")

#Mammal
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "mammal", column_name = "taxa")

#Reptile
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "reptile", column_name = "taxa")

#Amphibian
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "amphibian", column_name = "taxa")

#Fish
vennPreparation(area_list = list_search_voc, data = corpus_final, taxa_name = "fish", column_name = "taxa")

