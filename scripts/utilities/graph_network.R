# define a function to graph a given network using tkplot
graph_network <- function(dataset, 
                          weights, 
                          from.country = 'From_country', 
                          to.country = 'To_country', 
                          plot_type = 'standard', 
                          main = '', 
                          layout = layout.fruchterman.reingold,
                          output_fpath = NA
                          ){
  # Construct Social Network
  G <- graph_from_data_frame(d = dataset[, c(from.country, to.country)], directed = T)
  # add in the points as weights
  E(G)$weight <- round(as.numeric(dataset[, weights]), 3)
  # check graph is weighted
  if (is_weighted(graph = G) == FALSE)
    stop('Graph is not weighted')
  if (plot_type == 'standard'){
    if (!is.na(output_fpath)){
      jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
    }
    plot(x = G, 
         main = main,
         layout = layout, 
         vertex.color = "orange", 
         vertex.label.color = "black", 
         vertex.size = 0.01, 
         vertex.label.cex = 0.3,
         edge.color = "grey", 
         edge.arrow.size = 0.09, 
         edge.label.cex = 0.3,
         edge.label = E(G)$weight, 
         edge.label.color = "black",
         annotate.plot = TRUE
         )
    if (!is.na(output_fpath)){
      dev.off()
    }
  } else if (plot_type == 'tkplot') {
    # interactive drag and place Social Network plot 
    tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
  }
  return(0)
}