
# tutoriel AFC : 
# http://www.sthda.com/french/articles/38-methodes-des-composantes-principales-dans-r-guide-pratique/74-afc-analyse-factorielle-des-correspondances-avec-r-l-essentiel/


df_journaux <- readRDS(file = "C:/Users/Jan Perret/Desktop/effet_taxize/profile_journaux_tab.rds")
df_journaux$journal <- as.character(df_journaux$journal)

View(df_journaux)

library(FactoMineR)
library(factoextra)
library(gplots)

# 1. convertir les données en tant que table
dt <- as.table(as.matrix(df_journaux[,2:9]), row.names = df_journaux$journal)
rownames(dt) <- df_journaux$journal

# 2. Graphique
balloonplot(t(dt), main = "journaux", xlab = "", ylab = "",
            label = TRUE, show.margins = FALSE)

# test du chi2 pour voir s'il existe une dependance significative entre les categories des lignes et des colonnes
chisq <- chisq.test(df_journaux[,2:9])
chisq
# p-value < 0.05 donc oui, les variables de ligne et de colonne sont statistiquement significativement associées 

df_journaux_2 <- df_journaux[,2:9]
rownames(df_journaux_2) <- df_journaux$journal


# on fait l'AFC
res.ca <- CA(df_journaux_2, graph = FALSE)

# on inspecte les eigenvalues
eig.val <- get_eigenvalue (res.ca)
eig.val
# super on peut garder uniquement les 2 premiers axes !


# graphique AFC basique avec les lignes et les colonnes representes sur le meme graphique
fviz_ca_biplot(res.ca, repel = TRUE)

##### ca c'est le graphique qu'on met dans le diapo


# Colorer en fonction du cos2
fviz_ca_row(res.ca, col.row = "cos2",
             gradient.cols = c ("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE)
# plus c'est proche de 1 mieux le journal est represente dans l'espace des 2 dimensions


# biplot de contributions
fviz_ca_biplot(res.ca, map = "colgreen", arrow = c (TRUE, FALSE),
                repel = TRUE)
# je ne sais pas trop comment interpreter ce graphique




