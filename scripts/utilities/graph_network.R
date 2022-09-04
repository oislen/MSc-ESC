# define a function to graph a given network using tkplot
graph_network <- function(dataset, weights){
  # Construct Social Network
  G <- graph_from_data_frame(d = dataset[, c('From_country', 'To_country')], directed = T)
  # add in the points as weights
  E(G)$weight <- as.numeric(dataset[, weights])
  # check graph is weighted
  if (is_weighted(graph = G) == FALSE)
    stop('Graph is not weighted')
  # interactive drag and place Social Network plot 
  tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
  return(0)
}