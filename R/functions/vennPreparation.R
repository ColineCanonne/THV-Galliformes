vennPreparation <- function(area_list, data, taxa_name, column_name, folder_name) {
  # area_list contient les noms des aires
  # Il doit y avoir 5 noms dans area_list
  # column_name est le nom de column dans laquelle chercher taxa_name
  # folder_name est le nom du dossier dans lequel enregistrer la figure
  
  
  tab <- data[grepl(taxa_name, data[, which(colnames(corpus) == column_name)]), ]
  tab <- data.frame(table(tab$search_voc))
  tab[, 1] <- as.character(tab[, 1])
  
  for (i in seq_len(nrow(tab))) {
    indice <- NULL
    
    if (grepl(area_list[1], tab[i, 1])) {
      indice <- c(indice, "1")
    }
    
    if (grepl(area_list[2], tab[i, 1])) {
      indice <- c(indice, "2")
    }
    
    if (grepl(area_list[3], tab[i, 1])) {
      indice <- c(indice, "3")
    }
    
    if (grepl(area_list[4], tab[i, 1])) {
      indice <- c(indice, "4")
    }
    
    if (grepl(area_list[5], tab[i, 1])) {
      indice <- c(indice, "5")
    }
    
    if (length(indice) == 0) {
      indice <- c("0")
    }
    
    valeur <- as.character(indice[1])
    if (length(indice) > 1) {
      
      for (j in 2:length(indice)) {
        valeur <- as.character(paste0(valeur, indice[j]))
      }
    }
    
    tab[i, 1] <- as.character(valeur)
  }
  
  area1 <- sum(tab[grepl("1", tab[, 1]), 2])
  area2 <- sum(tab[grepl("2", tab[, 1]), 2])
  area3 <- sum(tab[grepl("3", tab[, 1]), 2])
  area4 <- sum(tab[grepl("4", tab[, 1]), 2])
  area5 <- sum(tab[grepl("5", tab[, 1]), 2])
  
  n12 <- sum(tab[grepl("12", tab[, 1]), 2])
  n13 <- sum(tab[which(tab[, 1] == "13"), 2],
             tab[which(tab[, 1] == "123"), 2],
             tab[which(tab[, 1] == "134"), 2],
             tab[which(tab[, 1] == "135"), 2],
             tab[which(tab[, 1] == "1234"), 2],
             tab[which(tab[, 1] == "1235"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n14 <- sum(tab[which(tab[, 1] == "14"), 2],
             tab[which(tab[, 1] == "124"), 2],
             tab[which(tab[, 1] == "134"), 2],
             tab[which(tab[, 1] == "145"), 2],
             tab[which(tab[, 1] == "1234"), 2],
             tab[which(tab[, 1] == "1245"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n15 <- sum(tab[which(tab[, 1] == "15"), 2],
             tab[which(tab[, 1] == "125"), 2],
             tab[which(tab[, 1] == "135"), 2],
             tab[which(tab[, 1] == "145"), 2],
             tab[which(tab[, 1] == "1235"), 2],
             tab[which(tab[, 1] == "1245"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n23 <- sum(tab[which(tab[, 1] == "23"), 2],
             tab[which(tab[, 1] == "123"), 2],
             tab[which(tab[, 1] == "234"), 2],
             tab[which(tab[, 1] == "235"), 2],
             tab[which(tab[, 1] == "1234"), 2],
             tab[which(tab[, 1] == "1235"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n24 <- sum(tab[which(tab[, 1] == "24"), 2],
             tab[which(tab[, 1] == "124"), 2],
             tab[which(tab[, 1] == "234"), 2],
             tab[which(tab[, 1] == "245"), 2],
             tab[which(tab[, 1] == "1234"), 2],
             tab[which(tab[, 1] == "1245"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n25 <- sum(tab[which(tab[, 1] == "25"), 2],
             tab[which(tab[, 1] == "125"), 2],
             tab[which(tab[, 1] == "235"), 2],
             tab[which(tab[, 1] == "245"), 2],
             tab[which(tab[, 1] == "1235"), 2],
             tab[which(tab[, 1] == "1245"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n34 <- sum(tab[which(tab[, 1] == "34"), 2],
             tab[which(tab[, 1] == "134"), 2],
             tab[which(tab[, 1] == "234"), 2],
             tab[which(tab[, 1] == "345"), 2],
             tab[which(tab[, 1] == "1234"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n35 <- sum(tab[which(tab[, 1] == "35"), 2],
             tab[which(tab[, 1] == "135"), 2],
             tab[which(tab[, 1] == "235"), 2],
             tab[which(tab[, 1] == "345"), 2],
             tab[which(tab[, 1] == "1235"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  n45 <- sum(tab[which(tab[, 1] == "45"), 2],
             tab[which(tab[, 1] == "145"), 2],
             tab[which(tab[, 1] == "245"), 2],
             tab[which(tab[, 1] == "345"), 2],
             tab[which(tab[, 1] == "1245"), 2],
             tab[which(tab[, 1] == "1345"), 2],
             tab[which(tab[, 1] == "2345"), 2],
             tab[which(tab[, 1] == "12345"), 2])
  
  n123 <- sum(tab[which(tab[, 1] == "123"), 2],
              tab[which(tab[, 1] == "1234"), 2],
              tab[which(tab[, 1] == "1235"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n124 <- sum(tab[which(tab[, 1] == "124"), 2],
              tab[which(tab[, 1] == "1234"), 2],
              tab[which(tab[, 1] == "1245"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n125 <- sum(tab[which(tab[, 1] == "125"), 2],
              tab[which(tab[, 1] == "1235"), 2],
              tab[which(tab[, 1] == "1245"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n134 <- sum(tab[which(tab[, 1] == "134"), 2],
              tab[which(tab[, 1] == "1234"), 2],
              tab[which(tab[, 1] == "1345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n135 <- sum(tab[which(tab[, 1] == "135"), 2],
              tab[which(tab[, 1] == "1235"), 2],
              tab[which(tab[, 1] == "1345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n145 <- sum(tab[which(tab[, 1] == "145"), 2],
              tab[which(tab[, 1] == "1245"), 2],
              tab[which(tab[, 1] == "1345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n234 <- sum(tab[which(tab[, 1] == "234"), 2],
              tab[which(tab[, 1] == "1234"), 2],
              tab[which(tab[, 1] == "2345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n235 <- sum(tab[which(tab[, 1] == "235"), 2],
              tab[which(tab[, 1] == "1235"), 2],
              tab[which(tab[, 1] == "2345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n245 <- sum(tab[which(tab[, 1] == "245"), 2],
              tab[which(tab[, 1] == "1245"), 2],
              tab[which(tab[, 1] == "2345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  n345 <- sum(tab[which(tab[, 1] == "345"), 2],
              tab[which(tab[, 1] == "1345"), 2],
              tab[which(tab[, 1] == "2345"), 2],
              tab[which(tab[, 1] == "12345"), 2])
  
  n1234 <- sum(tab[which(tab[, 1] == "1234"), 2],
               tab[which(tab[, 1] == "12345"), 2])
  n1235 <- sum(tab[which(tab[, 1] == "1235"), 2],
               tab[which(tab[, 1] == "12345"), 2])
  n1245 <- sum(tab[which(tab[, 1] == "1245"), 2],
               tab[which(tab[, 1] == "12345"), 2])
  n1345 <- sum(tab[which(tab[, 1] == "1345"), 2],
               tab[which(tab[, 1] == "12345"), 2])
  n2345 <- sum(tab[which(tab[, 1] == "2345"), 2],
               tab[which(tab[, 1] == "12345"), 2])
  
  n12345 <- tab[which(tab[, 1] == "12345"), 2]
    
  # Save pdf
  pdf(file = paste0("./output/plots/", folder_name, "/", taxa_name, "_venn.pdf"),
      width = 11.69,
      height = 8.27)
  draw.quintuple.venn(area1,
                      area2,
                      area3,
                      area4,
                      area5,
                      n12,
                      n13,
                      n14,
                      n15,
                      n23,
                      n24,
                      n25,
                      n34,
                      n35,
                      n45,
                      n123,
                      n124,
                      n125,
                      n134,
                      n135,
                      n145,
                      n234,
                      n235,
                      n245,
                      n345,
                      n1234,
                      n1235,
                      n1245,
                      n1345,
                      n2345,
                      n12345,
                      category = rep("", 5),
                      lwd = rep(2, 5),
                      lty = rep("solid", 5),
                      col = rep("black", 5),
                      fill = c("deepskyblue", "orchid1", "gold",
                               "tan1", "seagreen2"),
                      alpha = rep(0.5, 5),
                      label.col = rep("black", 31),
                      cex = rep(1, 31),
                      fontface = rep("plain", 31),
                      fontfamily = rep("serif", 31),
                      cat.pos = c(0, 287.5, 215, 145, 70),
                      cat.dist = rep(0.2, 5),
                      cat.col = rep("black", 5),
                      cat.cex = rep(1, 5),
                      cat.fontface = rep("plain", 5),
                      cat.fontfamily = rep("serif", 5),
                      cat.just = rep(list(c(0.5, 0.5)), 5),
                      rotation.degree = 0,
                      rotation.centre = c(0.5, 0.5),
                      ind = TRUE,
                      cex.prop = NULL,
                      print.mode = "raw",
                      sigdigs = 3,
                      direct.area = FALSE,
                      area.vector = 0)
  dev.off()
}