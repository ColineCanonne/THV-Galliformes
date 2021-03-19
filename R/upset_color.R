## Import des packages
library(ggplot2)
library(plyr)
library(dplyr)
library(gridExtra)
library(grid)

# Import du corpus de monitoring
corpus <- readRDS("./output/text/monitoring/corpus_final.rds")

## Default upset() arguments
nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1

## Initial inputs
name_taxa_list <- c("amphibian", "bird", "fish", "fungi", "invertebrate", "mammal", "plant", "reptile")

Art_vertebrate_list <- listNumAccess(corpus, "vertebrate", "big_taxa")
Art_invertebrate_list <- listNumAccess(corpus, "invertebrate", "big_taxa")
Art_plant_list <- listNumAccess(corpus, "plant", "big_taxa")
Art_fungi_list <- listNumAccess(corpus, "fungi", "big_taxa")
listInput <- list(vertebrate = Art_vertebrate_list,
                  invertebrate = Art_invertebrate_list,
                  plant = Art_plant_list,
                  fungi = Art_fungi_list)

taxons <- fromList(listInput)

## comma -> semicolon
data = taxons; sets = name_taxa_list 
order.by = "freq"; matrix.color="blue"
sets.bar.color = c("darkorange1", "cyan3", "darkorchid2", "seagreen3")
mainbar.y.label = "Taxa intersections" ; sets.x.label = "Articles per taxa"

Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                             shading_data, shade_alpha){
  
  if(length(text_scale) == 1){
    name_size_scale <- text_scale
  }
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    name_size_scale <- text_scale[5]
  }
  
  Mat_data$line_col <- 'black'
  
  Matrix_plot <- (ggplot()
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(colour = "gray0",
                                                     size = 7*name_size_scale, hjust = 0.4),
                          panel.grid.major = element_blank(),
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels, expand = c(0,0))
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                  + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                              ymin = "y_min", ymax = "y_max"),
                              fill = shading_data$shade_color, alpha = shade_alpha)
                  + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                         colour = "line_col"), size = line_size)
                  + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                               size= point_size, alpha = Mat_data$alpha, shape=16)
                  + scale_color_identity())
  Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
  return(Matrix_plot)
}

## Modified internal upset() code

startend <- UpSetR:::FindStartEnd(data)
first.col <- startend[1]
last.col <- startend[2]

if(color.pal == 1){
  palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
               "#7F7F7F", "#BCBD22", "#17BECF")
} else{
  palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
               "#CC79A7")
}

if(is.null(intersections) == F){
  Set_names <- unique((unlist(intersections)))
  Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
  New_data <- UpSetR:::Wanted(data, Sets_to_remove)
  Num_of_set <- UpSetR:::Number_of_sets(Set_names)
  if(keep.order == F){
    Set_names <- UpSetR:::order_sets(New_data, Set_names)
  }
  All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                               cutoff, main.bar.color, Set_names)
} else if(is.null(intersections) == T){
  Set_names <- sets
  if(is.null(Set_names) == T || length(Set_names) == 0 ){
    Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
  }
  Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
  New_data <- UpSetR:::Wanted(data, Sets_to_remove)
  Num_of_set <- UpSetR:::Number_of_sets(Set_names)
  if(keep.order == F){
    Set_names <- UpSetR:::order_sets(New_data, Set_names)
  }
  All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                order.by, group.by, cutoff, empty.intersections, decreasing)
}
Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
labels <- UpSetR:::Make_labels(Matrix_setup)
#Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
#i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
#if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
#NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
att.x <- c(); att.y <- c();
if(is.null(attribute.plots) == F){
  for(i in seq_along(attribute.plots$plots)){
    if(length(attribute.plots$plots[[i]]$x) != 0){
      att.x[i] <- attribute.plots$plots[[i]]$x
    }
    else if(length(attribute.plots$plots[[i]]$x) == 0){
      att.x[i] <- NA
    }
    if(length(attribute.plots$plots[[i]]$y) != 0){
      att.y[i] <- attribute.plots$plots[[i]]$y
    }
    else if(length(attribute.plots$plots[[i]]$y) == 0){
      att.y[i] <- NA
    }
  }
}

BoxPlots <- NULL
if(is.null(boxplot.summary) == F){
  BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
  BoxPlots <- list()
  for(i in seq_along(boxplot.summary)){
    BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
  }
}

customAttDat <- NULL
customQBar <- NULL
Intersection <- NULL
Element <- NULL
legend <- NULL
EBar_data <- NULL
if(is.null(queries) == F){
  custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
  customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
  legend <- UpSetR:::GuideGenerator(queries, palette)
  legend <- UpSetR:::Make_legend(legend)
  if(is.null(att.x) == F && is.null(customDat) == F){
    customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
  }
  customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
}
if(is.null(queries) == F){
  Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
  Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                    All_Freqs, expression, Set_names, palette)
  Element <- UpSetR:::SeperateQueries(queries, 1, palette)
  EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
} else{
  Matrix_col <- NULL
}

Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)

## our modification
for(i in 1:4) {
  j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
  if(length(j) > 0) Matrix_layout$color[j] <- c("darkorange1", "cyan3", "darkorchid2", "seagreen3")[i]
}

  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
 
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 16000)
  
 
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
            .Data=list(
              Main_bar = Main_bar,
              Matrix = Matrix,
              Sizes = Sizes,
              labels = labels,
              mb.ratio = mb.ratio,
              att.x = att.x,
              att.y = att.y,
              New_data = New_data,
              expression = expression,
              att.pos = att.pos,
              first.col = first.col,
              att.color = att.color,
              AllQueryData = AllQueryData,
              attribute.plots = attribute.plots,
              legend = legend,
              query.legend = query.legend,
              BoxPlots = BoxPlots,
              Set_names = Set_names,
              set.metadata = set.metadata,
              set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_grand_taxon.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  ##################################################################
  
  #Pour les mots clefs
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus, "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus, "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus, "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus, "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus, "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"

  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 21000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_search_voc.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  ##################################################################
  
  #Upset Bird
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("bird",corpus$taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("bird",corpus$taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("bird",corpus$taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("bird",corpus$taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("bird",corpus$taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("deepskyblue", "orchid1", "tan1", "gold", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("deepskyblue", "orchid1", "tan1", "gold", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 5000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_bird.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  
  ##################################################################
  
  #Upset Mammal
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("mammal",corpus$taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("mammal",corpus$taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("mammal",corpus$taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("mammal",corpus$taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("mammal",corpus$taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 5000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_mammal.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  
  ##################################################################
  
  #Upset Fish
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("fish",corpus$taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("fish",corpus$taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("fish",corpus$taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("fish",corpus$taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("fish",corpus$taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 5000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_fish.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  ##################################################################
  
  #Upset Reptile
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("reptile",corpus$taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("reptile",corpus$taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("reptile",corpus$taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("reptile",corpus$taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("reptile",corpus$taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("deepskyblue", "gold", "orchid1", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("deepskyblue", "gold", "orchid1", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 2500)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_reptile.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  
  ##################################################################
  
  #Upset Amphibian
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("amphibian",corpus$taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("amphibian",corpus$taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("amphibian",corpus$taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("amphibian",corpus$taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("amphibian",corpus$taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 2000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_amphibian.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  ##################################################################
  
  #Upset Plant
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("plant",corpus$big_taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("plant",corpus$big_taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("plant",corpus$big_taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("plant",corpus$big_taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("plant",corpus$big_taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "gold", "deepskyblue", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "gold", "deepskyblue", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 5000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_plant.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  
  
  ##################################################################
  
  #Upset Fungi
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("fungi",corpus$big_taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("fungi",corpus$big_taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("fungi",corpus$big_taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("fungi",corpus$big_taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("fungi",corpus$big_taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 2000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_fungi.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()
  
  
  
  ##################################################################
  
  #Upset Invertebrate
  
  ## Default upset() arguments
  nsets = 5; nintersects = 40; sets = NULL; keep.order = F; set.metadata = NULL; intersections = NULL;
  matrix.color = "gray23"; main.bar.color = "gray23"; mainbar.y.label = "Intersection Size"; mainbar.y.max = NULL;
  sets.bar.color = "gray23"; sets.x.label = "Set Size"; point.size = 2.2; line.size = 0.7;
  mb.ratio = c(0.70,0.30); expression = NULL; att.pos = NULL; att.color = main.bar.color; order.by = 'freq';
  decreasing = T; show.numbers = "yes"; number.angles = 0; group.by = "degree";cutoff = NULL;
  queries = NULL; query.legend = "none"; shade.color = "gray88"; shade.alpha = 0.25; matrix.dot.alpha =0.5;
  empty.intersections = NULL; color.pal = 1; boxplot.summary = NULL; attribute.plots = NULL; scale.intersections = "identity";
  scale.sets = "identity"; text.scale = 1; set_size.angles = 0 ; set_size.show = FALSE ; set_size.number_size = 1
  
  ## Initial inputs
  list_search_voc <- c("monitor", "dynamic", "demograph", "trend", "viability")
  
  Art_monitor_list <- listNumAccess(corpus[which(grepl("invertebrate",corpus$big_taxa)), ], "monitor", "search_voc")
  Art_dynamic_list <- listNumAccess(corpus[which(grepl("invertebrate",corpus$big_taxa)), ], "dynamic", "search_voc")
  Art_demograph_list <- listNumAccess(corpus[which(grepl("invertebrate",corpus$big_taxa)), ], "demograph", "search_voc")
  Art_trend_list <- listNumAccess(corpus[which(grepl("invertebrate",corpus$big_taxa)), ], "trend", "search_voc")
  Art_viability_list <- listNumAccess(corpus[which(grepl("invertebrate",corpus$big_taxa)), ], "viability", "search_voc")
  listInput <- list(monitor = Art_monitor_list,
                    dynamic = Art_dynamic_list,
                    demograph = Art_demograph_list,
                    trend = Art_trend_list,
                    viability = Art_viability_list)
  
  search_keywords <- fromList(listInput)
  
  ## comma -> semicolon
  data = search_keywords; sets = list_search_voc
  order.by = "freq"; matrix.color="blue"
  sets.bar.color = c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")
  mainbar.y.label = "Keyword intersections" ; sets.x.label = "Articles per keyword"
  
  Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, text_scale, labels,
                               shading_data, shade_alpha){
    
    if(length(text_scale) == 1){
      name_size_scale <- text_scale
    }
    if(length(text_scale) > 1 && length(text_scale) <= 6){
      name_size_scale <- text_scale[5]
    }
    
    Mat_data$line_col <- 'black'
    
    Matrix_plot <- (ggplot()
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                            axis.text.x = element_blank(),
                            axis.ticks.x = element_blank(),
                            axis.ticks.y = element_blank(),
                            axis.text.y = element_text(colour = "gray0",
                                                       size = 7*name_size_scale, hjust = 0.4),
                            panel.grid.major = element_blank(),
                            panel.grid.minor = element_blank())
                    + xlab(NULL) + ylab("   ")
                    + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                         limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                         labels = labels, expand = c(0,0))
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                    + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                                ymin = "y_min", ymax = "y_max"),
                                fill = shading_data$shade_color, alpha = shade_alpha)
                    + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                           colour = "line_col"), size = line_size)
                    + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                                 size= point_size, alpha = Mat_data$alpha, shape=16)
                    + scale_color_identity())
    Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
    return(Matrix_plot)
  }
  
  ## Modified internal upset() code
  
  startend <- UpSetR:::FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  } else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                                 cutoff, main.bar.color, Set_names)
  } else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- UpSetR:::FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- UpSetR:::Remove(data, first.col, last.col, Set_names)
    New_data <- UpSetR:::Wanted(data, Sets_to_remove)
    Num_of_set <- UpSetR:::Number_of_sets(Set_names)
    if(keep.order == F){
      Set_names <- UpSetR:::order_sets(New_data, Set_names)
    }
    All_Freqs <- UpSetR:::Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                                  order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- UpSetR:::Create_matrix(All_Freqs)
  labels <- UpSetR:::Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- UpSetR:::IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- UpSetR:::BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- UpSetR:::SeperateQueries(queries, 2, palette)
    customDat <- UpSetR:::customQueries(New_data, custom.queries, Set_names)
    legend <- UpSetR:::GuideGenerator(queries, palette)
    legend <- UpSetR:::Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- UpSetR:::CustomAttData(customDat, Set_names)
    }
    customQBar <- UpSetR:::customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- UpSetR:::SeperateQueries(queries, 1, palette)
    Matrix_col <- UpSetR:::intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                                      All_Freqs, expression, Set_names, palette)
    Element <- UpSetR:::SeperateQueries(queries, 1, palette)
    EBar_data <-UpSetR:::ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  } else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- UpSetR:::Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  
  ## our modification
  for(i in 1:5) {
    j <- which(Matrix_layout$y == i & Matrix_layout$value == 1)
    if(length(j) > 0) Matrix_layout$color[j] <- c("orchid1", "deepskyblue", "gold", "tan1", "seagreen2")[i]
  }
  
  ## continuing with upset()
  
  Set_sizes <- UpSetR:::FindSetFreqs(New_data, first.col, Num_of_set, Set_names, keep.order)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- UpSetR:::intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- UpSetR:::intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                           expression, Set_names, palette)
    QElem_att_data <- UpSetR:::elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                                        palette)
  }
  AllQueryData <- UpSetR:::combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    output <- Make_set_metadata_plot(set.metadata, Set_names)
    set.metadata.plots <- output[[1]]
    set.metadata <- output[[2]]
    
    if(is.null(ShadingData) == FALSE){
      shade.alpha <- unique(ShadingData$alpha)
    }
  } else {
    set.metadata.plots <- NULL
  }
  if(is.null(ShadingData) == TRUE){
    ShadingData <- UpSetR:::MakeShading(Matrix_layout, shade.color)
  }
  
  
  
  
  Main_bar <- suppressMessages(UpSetR:::Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                                                      mainbar.y.max, scale.intersections, text.scale, attribute.plots))
  
  
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             text.scale, labels, ShadingData, shade.alpha)
  
  Sizes <- UpSetR:::Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label, 
                                   scale.sets, text.scale, set_size.angles, set_size.show,
                                   set_size.number_size = 500, set_size.scale_max = 10000)
  
  
  # Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
  #                expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
  #                legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
  graphique <- structure(class = "upset",
                         .Data=list(
                           Main_bar = Main_bar,
                           Matrix = Matrix,
                           Sizes = Sizes,
                           labels = labels,
                           mb.ratio = mb.ratio,
                           att.x = att.x,
                           att.y = att.y,
                           New_data = New_data,
                           expression = expression,
                           att.pos = att.pos,
                           first.col = first.col,
                           att.color = att.color,
                           AllQueryData = AllQueryData,
                           attribute.plots = attribute.plots,
                           legend = legend,
                           query.legend = query.legend,
                           BoxPlots = BoxPlots,
                           Set_names = Set_names,
                           set.metadata = set.metadata,
                           set.metadata.plots = set.metadata.plots)
  )
  
  # save figures in PDF
  pdf(file = "./output/plots/upset_invert.pdf", width = 11.69, height = 8.27)
  graphique
  dev.off()