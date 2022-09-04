
######################################################################################################################
## SECTION 1 - DERIVING THE VOTING BLOCS #############################################################################
######################################################################################################################

# This section is specifically for generating the the historic voting blocs

# The following method for identify voting blocs is quote in multiple academic papers
# It influences my research as I intend to use the voting blocs as an independant variable
# to explain score

# I shall attempt to cluster the countries into voting blocs using the historic avergae vote
# The communities will be evaluated on modularity
# The modularity of a graph with respect to some division (or vertex types) measures how good the division is. 
# Thus the high the modularity the greater the division between communities

#-- Libraries --#

# Load in relevant libraries
library(igraph)
library(dplyr)

############
#-- Data --#
############

# set the working directory
setwd('C:/Users/User/Documents/GitHub/MSc-ESC')

# load in the historic voting data for deriving the voting blocs
past_voting_data <- read.csv(file = "Data/ESC_hist_voting_data.csv", header = T)

######################
#-- Data Profiling --#
######################

# the head of the data
head(past_voting_data)

# structure of the data
str(past_voting_data)

# summary statistics of the data
summary(past_voting_data)

# there is no missing data
anyNA(past_voting_data)

#########################################
#-- Construct the average vote matrix --#
#########################################

# Average over the voting data
pvd <- past_voting_data %>% 
       select(From.country, To.country, Points) %>%
       group_by(From.country, To.country) %>%
       summarise(Average.Points = mean(Points)) %>%
       as.data.frame()

# write the average voting data to a csv file
write.csv(x = pvd, file = "Data/Reference_Data/average_points.csv", row.names = F)

##########################################
#-- Construct the Average Points Graph --#
##########################################

# graph the data based on the weight of the average point score
# assumption: an average point score of 8 or more is a sign of bloc voting
G <- graph_from_data_frame(d = pvd[pvd$Average.Points >= 8, 1:2], directed = T)
E(G)$weight <- as.numeric(pvd[pvd$Average.Points >= 8, 3])
# check the graph is weighted
is_weighted(G)

###############################
#-- Derive the Voting Blocs --#
###############################

#--- Edge Betweeness Clsutering --#

# Perform Edge between clustering
cluster_edge_betweenness(graph = G, weights = E(G)$weight)

# Extract the voting blocs
# 21 groups
# global modularity = 0.057
com1 <- cluster_edge_betweenness(graph = G, weights = E(G)$weight)
com1df <- rbind(com1$names, com1$membership)
row.names(com1df) <- c("Country", "Group")
com1df

# construct a dendrogram (hierarchical clustering method)
plt = plot_dendrogram(cluster_edge_betweenness(graph = G, weights = E(G)$weight))

#-- Short Random Walks --#

# perform short random walks clustering
cluster_walktrap(graph = G, weights = E(G)$weight)

# Extract voting blocs
# 6 groups
# global modularity = 0.3
com6 <- cluster_walktrap(graph = G, weights = E(G)$weight)
com6 <- cluster_spinglass(graph = G, weights = E(G)$weight)
com6 <- cluster_infomap(graph = G, e.weights = E(G)$weight)
com6df <- rbind(com6$names, com6$membership)
row.names(com6df) <- c("Country", "Group")
com6df

# construct a dendrogram (hierarchical clustering method)
plot_dendrogram(cluster_walktrap(graph = G, weights = E(G)$weight), main = "Dendrogram of Short Random Walk Clustering")

########################################
#-- Construct voting blocs datadrame --#
########################################

# UPDATE: for the prepose of this research we shall only include hierarchical clustering methods
# (1) Edge-betweenness 
# (2) Short Random Walks

voting_bloc_data <- as.data.frame(cbind(com1$names, com1$membership, com6$membership))
colnames(voting_bloc_data) <- c("Country", "VBlocs1_EB", "VBlocs2_SRW")
head(voting_bloc_data)
summary(voting_bloc_data)

#-- Write the voting bloc data to a csv file --#

# Writing Voting Bloc Data to csv file
write.csv(x = voting_bloc_data, file = "Data/Reference_Data/voting_bloc_data.csv", row.names = F)