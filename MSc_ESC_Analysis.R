########################################################################################################################
## SCRIPT OVERVIEW ####################################################################################################
########################################################################################################################

# This script documents the analysis for the 2016 ESC for DIT's 2017 Autumn Masters Dissertation module  
# This script is divided up into six sections
# (1) Deriving Voting Blocs................................lines 69 - 170
# (2) Exploratory Analysis.................................lines 171 - 1403
# (3) Data Processing......................................lines 1404 - 2369
# (4) Data Modelling.......................................lines 2370 - 2693
# (5) Model Evaluation.....................................lines 2694 - 3073
# (6) Conclusions..........................................lines 3074 - 3038

# There is an introduction at the beginning of each section
# which outlines the objectives for each section

# IMPORTANT NOTE:
# There are two .csv data files to be loaded into this R script
# These data files can be located as an excel spreadsheet on github
# The results of the first section on deriving the voting blocs have already been loaded into the raw script
# This analysis should run without issues provided the data files are correctly loaded into R

###################
## Prelimineries ##
###################

#-- Libraries --#

# Load in relevant libraries
# igraph will be used for SNA
library(igraph)
# ggplot2 will be used for data visuaisation
library(ggplot2)
# gridExTRA allows for side by side charts
library(gridExtra)
# the dplyr library will be used to subset and average over the data
library(dplyr)
# the car library will be used for evaluting the models and for a box-cox transformation of the response variables
library(car)
# the MASS library will be used to derive the studenisted residuals from the models
library(MASS)
# the nortest library will be used for the Anderson-Darling Normality Test
library(nortest)

#-- Data --#

# getwd()
getwd()
# set the working directory
setwd(dir = "C:/Users/Margaret/Documents/Oisin/Education/MSc Computing Data Analytics/Semester 3/Data")
# load in the historic voting data for deriving the voting blocs
past_voting_data <- read.csv(file = "Reference Data/historic_voting_data.csv", header = T)
# load in the raw ESC 2016 data for the analysis
ESCdata <- read.csv(file = "NEWESC_2016_voting_data.csv", header = T)

#-- Some Intial Data Processing --#

# Some of the numeric music features need to be redefined as nominal variables
# the variables are key, mode and time signature
ESCdata$key <- as.factor(ESCdata$key)
ESCdata$mode <- as.factor(ESCdata$mode)
ESCdata$time_signature <- as.factor(ESCdata$time_signature)
ESCdata$VBlocs1_FC <- as.factor(ESCdata$VBlocs1_FC)
ESCdata$VBlocs2_FC <- as.factor(ESCdata$VBlocs2_FC)
ESCdata$VBlocs1_TC <- as.factor(ESCdata$VBlocs1_TC)
ESCdata$VBlocs2_TC <- as.factor(ESCdata$VBlocs2_TC)

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

#-- quick Data Profiling --#

# the head of the data
head(past_voting_data)
# structure of the data
str(past_voting_data)
# summary statistics of the data
summary(past_voting_data)
# there is no missing data
anyNA(past_voting_data)

#-- Construct the average vote matrix --#

# Average over the voting data
pvd <- past_voting_data %>% 
  select(From.country, To.country, Points) %>%
  group_by(From.country, To.country) %>%
  summarise(Average.Points = mean(Points))
head(pvd)
pvd <- as.data.frame(pvd)
# write the average voting data to a csv file
write.csv(x = pvd, file = "Reference Data/average_points.csv", row.names = F)

#-- Construct the Average Points Graph --#

# construct the graph the data
# instead of using all the data, we will graph the data based on the weight of the average point score
# this arbitary, but I am going to say that an average point score of 8 or more is a sign of bloc voting
G <- graph_from_data_frame(d = pvd[pvd$Average.Points >= 8, 1:2], directed = T)
E(G)$weight <- as.numeric(pvd[pvd$Average.Points >= 8, 3])
# check the graph is weighted
is_weighted(G)

#-- Derive the Voting Blocs using cluster analysis --#

# Edge between clustering
cluster_edge_betweenness(graph = G, weights = E(G)$weight)
# 21 groups
# global modularity = 0.057
com1 <- cluster_edge_betweenness(graph = G, weights = E(G)$weight)
com1df <- rbind(com1$names, com1$membership)
row.names(com1df) <- c("Country", "Group")
com1df
# as edge between clustering is a hierarchical clustering method we can construct a dendrogram
plot_dendrogram(cluster_edge_betweenness(graph = G, weights = E(G)$weight))

# short random walks clustering
cluster_walktrap(graph = G, weights = E(G)$weight)
# 6 groups
# global modularity = 0.3
com6 <- cluster_walktrap(graph = G, weights = E(G)$weight)
com6 <- cluster_spinglass(graph = G, weights = E(G)$weight)
com6 <- cluster_infomap(graph = G, e.weights = E(G)$weight)
com6df <- rbind(com6$names, com6$membership)
row.names(com6df) <- c("Country", "Group")
com6df
# as random walks clustering is a hierarchical clustering method we can construct a dendrogram
plot_dendrogram(cluster_walktrap(graph = G, weights = E(G)$weight), main = "Dendrogram of Short Random Walk Clustering")

# NOTE: Infomap Clustering groups = Label Propagation Clustering groups = Random Walks Clustering Groups
# NOTE: Optimal Community Structure Clustering groups = Statistical mechanics clustering groups

#-- Construct a data frame to hold the voting blocs data --#

# UPDATE: for the prepose of this research we shall only include hierarchical clustering methods
# (1) Edge-betweenness 
# (2) Short Random Walks

voting_bloc_data <- as.data.frame(matrix(nrow = 42, ncol = 3))
colnames(voting_bloc_data) <- c("Country", "VBlocs1_EB", "VBlocs2_SRW")
voting_bloc_data$Country <- com1$names
voting_bloc_data$VBlocs1_EB <- com1$membership
voting_bloc_data$VBlocs2_SRW <- com6$membership
head(voting_bloc_data)
summary(voting_bloc_data)

#-- Write the voting bloc data to a csv file --#

# Writing Voting Bloc Data to csv file
write.csv(x = voting_bloc_data, file = "voting_bloc_data.csv", row.names = F)

#-- Remove Unecessary Datasets --#

# To maintain a tiny R enviornment I shall remove some unneccessary data
rm(com1, com1df, com6, com6df, G, pvd)


########################################################################################################################
## SECTION 2 - EXPLORATORY ANALYSIS ####################################################################################
########################################################################################################################

# This section is specifically for visualising and deriving descriptive statistics on the data
# In order to understand the underlying the structures and patterns in the data

# A variety of descriptive statistics will be generated for both the numeric data and the categorical data
# Similarly a variety of visualisations will be generated for both the numeric data and the categorical data
# Further more some visualisation on the patterns of diaspora will be generated using Social Networks

######################################################################################################################
## Descriptive Statistics ------------------------------------------------------------------------------------------##
######################################################################################################################

# Here I shall derive relevant descriptive statistics for the exploratory analysis
# To do this I shall define two function which save the descriptive statistics in twp seperate data frames
# specifically for categorical data and numeric data

# DESCRIPTIVE STATISTICS FOR CATEGORICAL VARIABLES
factor_descriptive_statistics <- function(dataset) {
  # function that automatically prints relevant descriptive statistics for attributes in a given dataset
  library(moments)
  l = 1 # row index for categorical variables
  # create the data frame to hold the categorical descriptive statistics
  factor_num_col <- 6
  factor_num_row <- sum(sapply(X = dataset, FUN = function(x) is.factor(x)))
  factor_descriptive_statistics <- as.data.frame(matrix(nrow = factor_num_row, ncol = factor_num_col))
  colnames(factor_descriptive_statistics) <- c("nlevels", "1st mode", "1st mode %", "2nd mode", "2nd mode %", "NA %")
  for (i in 1:ncol(dataset)) {
    if (is.factor(dataset[,i])){
      # first write in the row name i.e. the attribute name
      rownames(factor_descriptive_statistics)[l] <- colnames(dataset)[i]
      # next compute the descriptive statistics for each cell in the row
      factor_descriptive_statistics[l, 1] <- nlevels(dataset[, i])
      factor_descriptive_statistics[l, 2] <- names(which.max(summary(dataset[,i])))
      factor_descriptive_statistics[l, 3] <- round(summary(dataset[, i])[which.max(summary(dataset[,i]))] * 100 / sum(summary(dataset[,i])), digits = 2)
      factor_descriptive_statistics[l, 4] <- names(sort(summary(dataset[, i]), decreasing = T)[2])
      factor_descriptive_statistics[l, 5] <- round(sort(summary(dataset[, i]), decreasing = T)[2] * 100/ sum(summary(dataset[,i])), digits = 2)
      factor_descriptive_statistics[l, 6] <- length(which(is.na(dataset[,i]))) * 100 / sum(summary(dataset[,i]))
      # print(paste("Categorical Attribute:", colnames(dataset)[i], sep = " "))
      # print(summary(dataset[,i]))
      l = l + 1
    } 
  }
  return(factor_descriptive_statistics)
}
# NOTE: could code up a table of descriptive statistics for continuous and decrete variables, metadata
# descriptive_statistics(dataset = train)
# factor_descriptive_statistics(dataset = reduceddata)
full_data_factor_descriptive_statistics_df <- factor_descriptive_statistics(dataset = ESCdata)
# View(full_data_factor_descriptive_statistics_df)
write.csv(x = full_data_factor_descriptive_statistics_df,
          file = "Exploratory Analysis/Descriptive Statistics/E.A._categorical_descriptive_statistics.csv")

# DESCRIPTIVE STATISTICS FOR CONTINUOUS FEATURES
numeric_descriptive_statistics <- function(dataset) {
  # IMPORTANT NOTE: all the descriptive statistics are calculated with the NA values removed
  # function that automatically prints relevant descriptive statistics for attributes in a given dataset
  library(moments)
  k = 1 # row index for numeric variables
  # create the data frame to hold the numeric descriptive statistics
  numeric_num_col <- 6
  numeric_num_row <- sum(sapply(X = dataset, FUN = function(x) is.numeric(x)))
  numeric_descriptive_statistics <- as.data.frame(matrix(nrow = numeric_num_row, ncol = numeric_num_col))
  colnames(numeric_descriptive_statistics) <- c("mean", "variance", "min", "max", "range", "NA %")
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])) {
      # first write in the row name i.e. the aattribute name
      rownames(numeric_descriptive_statistics)[k] <- colnames(dataset)[i]
      # next compute the descriptive statistics for each cell in the row
      numeric_descriptive_statistics[k, 1] <- round(mean(dataset[,i], na.rm = T),
                                                    digits = 2)
      numeric_descriptive_statistics[k, 2] <- round(var(dataset[,i], na.rm = T),
                                                    digits = 2)
      numeric_descriptive_statistics[k, 3] <- round(min(dataset[,i], na.rm = T),
                                                    digits = 2)
      numeric_descriptive_statistics[k, 4] <- round(max(dataset[,i], na.rm = T),
                                                    digits = 2)
      numeric_descriptive_statistics[k, 5] <- round(max(dataset[,i], na.rm = T) - min(dataset[,i], na.rm = T),
                                                    digits = 2)
      numeric_descriptive_statistics[k, 6] <- round(length(which(is.na(dataset[,i]))) * 100 / nrow(dataset),
                                                    digits = 3)
      k = k + 1
    }
    # print("############################")
  }
  return(numeric_descriptive_statistics)
}
full_data_numeric_descriptive_statistics_df <- numeric_descriptive_statistics(dataset = ESCdata)
# View(full_data_numeric_descriptive_statistics_df)
write.csv(x = full_data_numeric_descriptive_statistics_df,
          file = "Exploratory Analysis/Descriptive Statistics/E.A._numeric_descriptive_statistics.csv")

######################################################################################################################
## Individual Categorical Variable Bar Charts ----------------------------------------------------------------------##
######################################################################################################################

# In this section I shall generate a variety of data visualisations
# This allows us to see the underlying structures within each variable
# I shall generate bar charts for categorical variables

####################
## Voting Factors ##
####################

#-- From_country --#

# Bar Chart of From_country
ggplot(data = ESCdata, mapping = aes(x = From_country, fill = From_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of From_country", x = "From_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- To_country --#

# Bar Chart of To_country
ggplot(data = ESCdata, mapping = aes(x = To_country, fill = To_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of To_country", x = "To_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
# Points appears to have a uniform distribution

#-- Points --#

# Ordinal Response Variable Points
# Bar Chart of Points
ggplot(data = ESCdata, mapping = aes(x = as.factor(Points), fill = as.factor(Points))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Points", x = "Points", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
# Points appears to have a uniform distribution

#########################
## Competition Factors ##
#########################

#-- Round --#

# Bar Chart of Round
ggplot(data = ESCdata, mapping = aes(x = Round, fill = Round)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Round", x = "Round", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
# Round appears to be Uni-Modal

#-- Voting_Method --#

# Bar Chart of Voting_Method
ggplot(data = ESCdata, mapping = aes(x = Voting_Method, fill = Voting_Method)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Voting_Method", x = "Voting_Method", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
# Voting Method Appears to be Uniform

#-- Host_Nation --#

# Bar Chart of Host_Nation
ggplot(data = ESCdata, mapping = aes(x = Host_Nation, fill = Host_Nation)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Host_Nation", x = "Host_Nation", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- OOA --#

# Bar Chart of OOA
ggplot(data = ESCdata, mapping = aes(x = as.factor(OOA), fill = as.factor(OOA))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of OOA", x = "OOA", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

######################
## External Factors ##
######################

#-- Voting Blocks --#

# Bar Chart of VBlocs1_TC
plot1 <- ggplot(data = ESCdata, mapping = aes(x = VBlocs1_TC, fill = VBlocs1_TC)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of VBlocs1_TC", x = "VBlocs1_TC", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

# Bar Chart of VBlocs2_TC
plot2 <- ggplot(data = ESCdata, mapping = aes(x = VBlocs2_TC, fill = VBlocs2_TC)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of VBlocs2_TC", x = "VBlocs2_TC", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

# Bar Chart of VBlocs1_FC
plot3 <- ggplot(data = ESCdata, mapping = aes(x = VBlocs1_FC, fill = VBlocs1_FC)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of VBlocs1_FC", x = "VBlocs1_FC", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
# VBlocs1_TC is uni-modal

# Bar Chart of VBlocs2_FC
plot4 <- ggplot(data = ESCdata, mapping = aes(x = VBlocs2_FC, fill = VBlocs2_FC)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of VBlocs2_FC", x = "VBlocs2_FC", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

grid.arrange(plot1, plot2, plot3, plot4)

#-- ComVBlocs --#

# Bar Chart of ComVBlocs1
ggplot(data = ESCdata, mapping = aes(x = ComVBlocs1, fill = ComVBlocs1)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of ComVBlocs1", x = "ComVBlocs1", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

# Bar Chart of ComVBlocs2
ggplot(data = ESCdata, mapping = aes(x = ComVBlocs2, fill = ComVBlocs2)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of ComVBlocs2", x = "ComVBlocs2", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)


#-- LANGFAM --#

ggplot(data = ESCdata, mapping = aes(x = TC_LANGFAM, fill = TC_LANGFAM)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of TC_LANGFAM", x = "TC_LANGFAM", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- ComLANGFAM --#

# Bar Chart of ComLANGFAM
ggplot(data = ESCdata, mapping = aes(x = ComLANGFAM, fill = ComLANGFAM)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of ComLANGFAM", x = "ComLANGFAM", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- Neighbours --#

# Bar Chart of Neighbours
ggplot(data = ESCdata, mapping = aes(x = Neighbours, fill = Neighbours)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Neighbours", x = "Neighbours", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- TC_NumNeigh --#

# Bar Chart of TC_NumNeigh
ggplot(data = ESCdata, mapping = aes(x = as.factor(TC_NumNeigh), fill = as.factor(TC_NumNeigh))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of TC_NumNeigh", x = "TC_NumNeigh", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#########################
## Performance Factors ##
#########################

#-- ComSONGLAN --#

# Bar Chart of ComSONGLAN
ggplot(data = ESCdata, mapping = aes(x = ComSONGLAN, fill = ComSONGLAN)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of ComSONGLAN", x = "ComSONGLAN", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- key --#

# Bar Chart of key
ggplot(data = ESCdata, mapping = aes(x = as.factor(key), fill = as.factor(key))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of key", x = "key", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- mode --#

# Bar Chart of mode
ggplot(data = ESCdata, mapping = aes(x = as.factor(mode), fill = as.factor(mode))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of mode", x = "mode", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- time_signature --#

# Bar Chart of time_signature
ggplot(data = ESCdata, mapping = aes(x = as.factor(time_signature), fill = as.factor(time_signature))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of time_signature", x = "time_signature", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- TC_PerfType --#

# Bar Chart of TC_PerfType
ggplot(data = ESCdata, mapping = aes(x = TC_PerfType, fill = TC_PerfType)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of TC_PerfType", x = "TC_PerfType", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

#-- TC_SingerGender --#

# Bar Chart of TC_SingerGender
ggplot(data = ESCdata, mapping = aes(x = TC_SingerGender, fill = TC_SingerGender)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of TC_SingerGender", x = "TC_SingerGender", y = "Count") + 
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)

######################################################################################################################
## Individual Numeric Variable Plots -------------------------------------------------------------------------------##
######################################################################################################################

# In this section I shall generate a variety of data visualisations
# This allows us to see the underlying structures within each variable
# I shall generate histograms for the numeric variables

#-- Average Point Score --#

# Histogram of Average_Points
ggplot(data = ESCdata, mapping = aes(x = Average_Points)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of Average_Points", x = "Average_Points", y = "Total") + 
  theme_minimal()
# Average points appear to be approximately normal

########################
## Migration Patterns ##
########################

#-- FC_NonCOB --#

# Histogram of FC_NonCOB
ggplot(data = ESCdata, mapping = aes(x = FC_NonCOB)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_NonCOB", x = "FC_NonCOB", y = "Total") + 
  theme_minimal()
# FC_Non_COB is very right skewed

#-- FC_NonCitzens --#

# Histogram of FC_NonCitzens
ggplot(data = ESCdata, mapping = aes(x = FC_NonCitzens)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_NonCitzens", x = "FC_NonCitzens", y = "Total") + 
  theme_minimal()
# FC_NonCitzens is very right skewed

#-- FC_COB --#

# Histogram of FC_COB
ggplot(data = ESCdata, mapping = aes(x = FC_COB)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_COB", x = "FC_COB", y = "Total") + 
  theme_minimal()
# FC_COB is very right skewed

#-- FC_Citizens --#

# Histogram of FC_Citizens
ggplot(data = ESCdata, mapping = aes(x = FC_Citizens)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_Citizens", x = "FC_Citizens", y = "Total") + 
  theme_minimal()
# FC_Citizens is right skewed

#-- FC_Population --#

# Histogram of FC_Population
ggplot(data = ESCdata, mapping = aes(x = FC_Population)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_Population", x = "FC_Population", y = "Total") + 
  theme_minimal()
# FC_Population is right skewed

#-- METRIC_COB --#

# Histogram of METRIC_COB
plot1 <- ggplot(data = ESCdata, mapping = aes(x = METRIC_COB)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of METRIC_COB", x = "METRIC_COB", y = "Total") + 
  theme_minimal()
# METRIC_COB is right skewed

#-- METRIC_Citizens --#

# Histogram of METRIC_Citizens
plot2 <- ggplot(data = ESCdata, mapping = aes(x = METRIC_Citizens)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of METRIC_Citizens", x = "METRIC_Citizens", y = "Total") + 
  theme_minimal()
# METRIC_Citizens is right skewed

#-- METRIC_COBCit --#

# Histogram of METRIC_COBCit
plot3 <- ggplot(data = ESCdata, mapping = aes(x = METRIC_COBCit)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of METRIC_COBCit", x = "METRIC_COBCit", y = "Total") + 
  theme_minimal()
# METRIC_COBCit is riht skewed

grid.arrange(plot1, plot2, plot3)

###################
## Music Factors ##
###################

#-- Danceability --#

# Histogram of Danceability
ggplot(data = ESCdata, mapping = aes(x = danceability)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of Danceability", x = "Danceability", y = "Total") + 
  theme_minimal()

#-- Loudiness --#

# Histogram of Loudiness
ggplot(data = ESCdata, mapping = aes(x = loudiness)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of loudiness", x = "loudiness", y = "Total") + 
  theme_minimal()

#-- Energy --#

# Histogram of Energy
ggplot(data = ESCdata, mapping = aes(x = energy)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of energy", x = "energy", y = "Total") + 
  theme_minimal()

#-- Speechiness --#

# Histogram of Speechiness
ggplot(data = ESCdata, mapping = aes(x = speechiness)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of speechiness", x = "speechiness", y = "Total") + 
  theme_minimal()

#-- Acousticness --#

# Histogram of Acousticness
ggplot(data = ESCdata, mapping = aes(x = acousticness)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of acousticness", x = "acousticness", y = "Total") + 
  theme_minimal()

#-- instrumentalness --#

# Histogram of instrumentalness
ggplot(data = ESCdata, mapping = aes(x = instrumentalness)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of instrumentalness", x = "instrumentalness", y = "Total") + 
  theme_minimal()

#-- Valence --#

# Histogram of Valence
ggplot(data = ESCdata, mapping = aes(x = valence)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of valence", x = "valence", y = "Total") + 
  theme_minimal()

#-- Tempo --#

# Histogram of tempo
ggplot(data = ESCdata, mapping = aes(x = tempo)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of tempo", x = "tempo", y = "Total") + 
  theme_minimal()

#-- Duration_ms --#

# Histogram of duration_ms
ggplot(data = ESCdata, mapping = aes(x = duration_ms)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of duration_ms", x = "duration_ms", y = "Total") + 
  theme_minimal()

#-- FC_GDP_mil --#

# Histogram of FC_GDP_mil
ggplot(data = ESCdata, mapping = aes(x = FC_GDP_mil)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of FC_GDP_mil", x = "FC_GDP_mil", y = "Total") + 
  theme_minimal()
# FC_GDP_mil is right skewed

#-- TC_GDP_mil --#

# Histogram of TC_GDP_mil
ggplot(data = ESCdata, mapping = aes(x = TC_GDP_mil)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of TC_GDP_mil", x = "TC_GDP_mil", y = "Total") + 
  theme_minimal()
# TC_GDP_mil is right skewed

#-- GDP_PROP --#

# Histogram of GDP_PROP
ggplot(data = ESCdata, mapping = aes(x = GDP_PROP)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of GDP_PROP", x = "GDP_PROP", y = "Total")  + 
  theme_minimal()
# GDP_PROP is right skewed

#-- CAP_DIST_km --#

# Histogram of CAP_DIST_km
ggplot(data = ESCdata, mapping = aes(x = CAP_DIST_km)) + 
  geom_histogram(col = "black", fill = "steelblue") + 
  labs(title = "Histogram of CAP_DIST_km", x = "CAP_DIST_km", y = "Total") + 
  theme_minimal()
# CAP_DIST_km is tri-model, this represents the three regions
# (1) Europe
# (2) Caucasus 
# (3) Austrailia

######################################################################################################################
## CHI-SQUARED TESTS OF ASSOCIATION --------------------------------------------------------------------------------##
######################################################################################################################

# In this section I shall conduct chi-squared tests of assocition
# for each categorical predictor variable and the response variable
# Hypothesis:
# Ho: x is independant of y
# Ha: x is associated with y

# There are two parts to it
# (1) Create the data frameto store the chi-squared tests
# (2) Fill the data frame with the relevant information

# First create a dataframe to store the relevent chi-squared test data
chi_sq_test_data <- subset(x = ESCdata,
                           select = c(From_country, To_country, Round,
                                      Voting_Method, Host_Nation, OOA,
                                      VBlocs1_FC, VBlocs2_FC,
                                      VBlocs1_TC, VBlocs2_TC,
                                      ComVBlocs1, ComVBlocs2,
                                      FC_LANGFAM, TC_LANGFAM, ComLANGFAM,
                                      Neighbours, TC_NumNeigh, TC_PerfType,
                                      TC_SingerGender, FC_SONGLANG, TC_SONGLANG,
                                      ComSONGLAN, key, mode, time_signature))
chisqtestdf <- as.data.frame(matrix(nrow = 25, 
                                    ncol = 4))
# rename the columns of the data frame
colnames(chisqtestdf) <- c("X", "Y", "P-Value", "Significant")

# Use a for loop to fill in the data frame
for (i in 1:25) {
  # Save the variables name being tested
  chisqtestdf[i,1] <- colnames(chi_sq_test_data)[i]
  chisqtestdf[i,2] <- "Points"
  # Conduct the chi-squared test and savethe p-value
  chisqtestdf[i,3] <- round(x = chisq.test(x = as.factor(chi_sq_test_data[,i]),
                                           y = as.factor(ESCdata$Points))$p.value,
                            digits = 5)
  chisqtestdf[i,4] <- ifelse(test = round(x = chisq.test(x = as.factor(chi_sq_test_data[,i]),
                                                         y = as.factor(ESCdata$Points))$p.value,
                                          digits = 5) < 0.05,
                             yes = "y",
                             no = "n")
}
# write the data frame to a csv file
write.csv(x = chisqtestdf,
          file = "Exploratory Analysis/Descriptive Statistics/E.A._chi_sq_tests_response.csv",
          row.names = F)

######################################################################################################################
## Correlation Scatter Plots ---------------------------------------------------------------------------------------##
######################################################################################################################

#-- Average_Points --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = Average_Points)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Average_Points", x = "Points", y = "Average_Points")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

########################
## Migration Patterns ##
########################

#-- FC_NonCOB --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_NonCOB)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_NonCOB", x = "Points", y = "FC_NonCOB")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- FC_NonCitzens --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_NonCitzens)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_NonCitizens", x = "Points", y = "FC_NonCitizens")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- FC_COB --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_COB)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_COB", x = "Points", y = "FC_COB")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- FC_Citizens --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_Citizens)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_Citizens", x = "Points", y = "FC_Citizens")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- FC_Population

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_Population)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_Population", x = "Points", y = "FC_Population")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- METRIC_COB --#

plot1 <- ggplot(data = ESCdata, mapping = aes(x = Points, y = METRIC_COB)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs METRIC_COB", x = "Points", y = "METRIC_COB")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- METRIC_Citizens --#

plot2 <- ggplot(data = ESCdata, mapping = aes(x = Points, y = METRIC_Citizens)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs METRIC_Citizens", x = "Points", y = "METRIC_Citizens")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

grid.arrange(plot1, plot2, ncol = 2)

#-- METRIC_COBCit --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = METRIC_COBCit)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs METRIC_COBCit", x = "Points", y = "METRIC_COBCit")  +  
  geom_smooth(method ='lm', linetype = "dashed", color = "darkred", fill = "red")+
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()


##############################################
## Longatude, Latitude and Capital Distnces ##
##############################################

#-- FC_CAP_LON --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_CAP_LON)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_CAP_LON", x = "Points", y = "FC_CAP_LON")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- FC_CAP_LAT --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_CAP_LAT)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_CAP_LAT", x = "Points", y = "FC_CAP_LAT")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- TC_CAP_LON --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = TC_CAP_LON)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs TC_CAP_LON", x = "Points", y = "TC_CAP_LON")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- TC_CAP_LON --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = TC_CAP_LON)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs TC_CAP_LON", x = "Points", y = "TC_CAP_LON")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- TC_CAP_LAT --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = TC_CAP_LAT)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs TC_CAP_LAT", x = "Points", y = "TC_CAP_LAT")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- CAP_DIST_km --#

# Via research, Distance between capital cities does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = CAP_DIST_km)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs CAP_DIST_km", x = "Points", y = "CAP_DIST_km")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- OOA --#

# Via research, Order of Appearance does have an effect on points
ggplot(data = ESCdata, mapping = aes(x = Points, y = OOA)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs OOA", x = "Points", y = "OOA")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- TC_NumNeigh --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = TC_NumNeigh)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs TC_NumNeigh", x = "Points", y = "TC_NumNeigh")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) +
  scale_y_discrete(limits = c("2","3","4","5","6","7","8","9","10")) + 
  theme_minimal()

#-- FC_GDP_mil --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = FC_GDP_mil)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs FC_GDP_mil", x = "Points", y = "FC_GDP_mil")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- TC_GDP_mil --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = TC_GDP_mil)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs TC_GDP_mil", x = "Points", y = "TC_GDP_mil")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- GDP_PROP --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = GDP_PROP)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs GDP_PROP", x = "Points", y = "GDP_PROP")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Danceability --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = danceability)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Danceability", x = "Points", y = "danceability")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Energy --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = energy)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Energy", x = "Points", y = "Energy")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Loudiness --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = loudiness)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Loudiness", x = "Points", y = "Loudiness")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Speechiness --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = speechiness)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Speechiness", x = "Points", y = "Speechiness")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Acousticness --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = acousticness)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Acousticness", x = "Points", y = "Acousticness")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Instrumentalness --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = instrumentalness)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Instrumentalness", x = "Points", y = "Instrumentalness")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Liveliness --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = liveness)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Liveliness", x = "Points", y = "Liveliness")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Valence --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = valence)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Valence", x = "Points", y = "Valence")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Tempo --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = tempo)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Tempo", x = "Points", y = "Tempo")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()

#-- Duration --#

ggplot(data = ESCdata, mapping = aes(x = Points, y = duration_ms)) + 
  geom_point(shape = 16, colour = "blue") + 
  labs(title = "Scatterplot of Points vs Duration", x = "Points", y = "Duration")  +  
  geom_smooth(method ='lm', linetype = "dashed", color="darkred", fill = "red") +
  scale_x_discrete(limits = c("1","2","3","4","5","6","7","8","","10","","12")) + 
  theme_minimal()


######################################################################################################################
## Response Correlation Tests --------------------------------------------------------------------------------------##
######################################################################################################################

# Correlation Test
# Ho: x is not correlated with y
# Ha: x is correlated with y

# Select the relevant numeric data from the ESCdata data frame
cor_test_response_data <- subset(x = ESCdata,
                                 select = c(Average_Points, OOA, TC_NumNeigh,
                                            FC_NonCOB, FC_NonCitzens, FC_COB,
                                            FC_Citizens, FC_Population, METRIC_COB,
                                            METRIC_Citizens, METRIC_COBCit, FC_GDP_mil,
                                            TC_GDP_mil, GDP_PROP, FC_CAP_LAT, FC_CAP_LON,
                                            TC_CAP_LAT, TC_CAP_LON, CAP_DIST_km, danceability,
                                            energy, key, loudiness, mode, speechiness, acousticness,
                                            instrumentalness, liveness, valence, tempo, duration_ms))
# Define a function to perform Correlation Tests
# Create a data frame to hold the correlation test data
cor_test_df <- as.data.frame(matrix(nrow = 31, ncol = 5))
# Name the columns of the Correlation Test Data Frame
colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value", "Significant")
# run a for loop to populate the data frame
for (i in 1:31) {
  # Perform Correlation Test
  c.t. <- cor.test(x = as.numeric(cor_test_response_data[,i]),
                   y = as.numeric(ESCdata$Points),
                   na.action = "na.omit")
  # Fill in the X Variable Name
  cor_test_df[i, 1] <- colnames(cor_test_response_data)[i]
  # Fill in the Y Variable Name
  cor_test_df[i, 2] <- "Points"
  # Fill in the correlation
  cor_test_df[i, 3] <- round(c.t.$estimate, digits = 2)
  # Fill in the p-value
  cor_test_df[i, 4] <- round(c.t.$p.value, digits = 2)
  # Fill in the significance column
  cor_test_df[i, 5] <- ifelse(test = round(c.t.$p.value, digits = 5) < 0.05,
                              yes = "y",
                              no = "n")
}
# Write the cor_test_df to  csv file
write.csv(x = cor_test_df,
          file = "Exploratory Analysis/Descriptive Statistics/E.A._cor_tests_response.csv",
          row.names = F)

######################################################################################################################
## Predictor Correlation Plots and Tests ---------------------------------------------------------------------------##
######################################################################################################################

# IMPORTANT: Correlation does not imply causality

# Define a function to perform Correlation Tests
correlation_tests <- function (dataset) {
  # Create a data frame to hold the correlation test data
  data_frame_rows <- ncol(dataset)^2
  cor_test_df <- as.data.frame(matrix(nrow = data_frame_rows, ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:ncol(dataset)) {
    for (j in 1:ncol(dataset)) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,i],
                       y = dataset[,j],
                       na.action = "na.omit")
      # Fill in the X Variable Name
      cor_test_df[r, 1] <- colnames(dataset)[i]
      # Fill in the Y Variable Name
      cor_test_df[r, 2] <- colnames(dataset)[j]
      # Fill in the correlation
      cor_test_df[r, 3] <- round(c.t.$estimate, digits = 5)
      # Fill in the p-value
      cor_test_df[r, 4] <- round(c.t.$p.value, digits = 5)
      # Update the row index
      r = r + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}

#########################
## Competition Factors ##
#########################

# Correlation Matrix
cor(subset(x = ESCdata,
           select = c(Average_Points, OOA)), 
    use = "complete.obs")

# Scatter Plot Matrix
pairs(x = subset(x = ESCdata,
                 select = c(Average_Points, OOA)),
      main = "Correlation Matirx of Competition Factors")

# Correlation Tests
c.t. <- correlation_tests(dataset = subset(x = ESCdata,
                                           select = c(Average_Points, OOA)))
# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,]

#########################
## Performance Factors ##
#########################

# Correlation Matrix
cor(subset(x = ESCdata,
           select = c(danceability, energy, loudiness, speechiness,
                      acousticness, instrumentalness, liveness, valence,
                      tempo, duration_ms)), 
    use = "complete.obs")

# Scatter Plot Matrix
pairs(subset(x = ESCdata,
             select = c(danceability, energy, loudiness, speechiness,
                        acousticness, instrumentalness, liveness, valence,
                        tempo, duration_ms)),
      main = "Correlation Matirx of Performance Factors")

# Correlation Tests
c.t. <- correlation_tests(dataset = subset(x = ESCdata,
                                           select = c(danceability, energy, loudiness, speechiness,
                                                      acousticness, instrumentalness, liveness, valence,
                                                      tempo, duration_ms)))
# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,]


######################
## External Factors ##
######################

# Correlation Matrix
cor(x = subset(ESCdata,
               select = c(TC_NumNeigh, FC_NonCOB,
                          FC_NonCitzens, FC_COB, FC_Citizens, FC_Population,
                          METRIC_COB, METRIC_Citizens, METRIC_COBCit,
                          FC_GDP_mil, TC_GDP_mil, GDP_PROP, FC_CAP_LAT,
                          FC_CAP_LON, TC_CAP_LAT, TC_CAP_LON, CAP_DIST_km)), 
    use = "complete.obs")

# Scatter Plot Matrix
pairs(subset(ESCdata,
             select = c(TC_NumNeigh, FC_NonCOB,
                        FC_NonCitzens, FC_COB, FC_Citizens, 
                        FC_Population, METRIC_COB, METRIC_Citizens, 
                        METRIC_COBCit, FC_GDP_mil, TC_GDP_mil, 
                        GDP_PROP, FC_CAP_LAT, FC_CAP_LON, TC_CAP_LAT, 
                        TC_CAP_LON, CAP_DIST_km)),
      main = "Correlation Matirx of External Factors")

# Correlation Tests
c.t. <- correlation_tests(dataset = subset(ESCdata,
                                           select = c(TC_NumNeigh, FC_NonCOB,
                                                      FC_NonCitzens, FC_COB, FC_Citizens, 
                                                      FC_Population, METRIC_COB, METRIC_Citizens, 
                                                      METRIC_COBCit, FC_GDP_mil, TC_GDP_mil, 
                                                      GDP_PROP, FC_CAP_LAT, FC_CAP_LON, TC_CAP_LAT, 
                                                      TC_CAP_LON, CAP_DIST_km)))
# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,]
# write.csv(x = c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,],
#            file = "E.A.predictor_variables_correlations.csv")

######################################################################################################################
## Socai Networks --------------------------------------------------------------------------------------------------##
######################################################################################################################

networkdata <- subset(x = ESCdata[complete.cases(ESCdata),],
                      select = c(From_country, To_country, Points, Round, 
                                 Voting_Method, Average_Points, VBlocs1_FC,
                                 VBlocs1_TC, VBlocs2_FC, VBlocs2_TC, METRIC_COB,
                                 METRIC_Citizens, METRIC_COBCit))
# NOTE: that there is more data missing for METIC_Citizens than METRIC_COB
sum(!is.na(ESCdata$METRIC_COB))
sum(!is.na(ESCdata$METRIC_Citizens))
networkdata <- ESCdata[!is.na(ESCdata$METRIC_Citizens),]

# A variety of Social Networks based on different closeness metrics given a specified Voting_Method

##################
## Network Data ##
##################

#-- Televote vs Jury --#

# Here I shall explore the effect of the televote and the Jury Vote

#-- METRIC_COB --#

summary(networkdata$METRIC_COB)
# (1) Voting_Method = Televote
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COB > 0.1, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COB > 0.1, 3])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")

# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COB > 0.1, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COB > 0.1, 3])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")

plot(G, main = "Graph of High Diaspora & Points in Televote", 
     layout = layout.fruchterman.reingold, 
     edge.color = "grey", 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.size = 10, 
     edge.arrow.size = .5, 
     edge.label = E(G)$weight, 
     edge.label.color = "black")

#-- METRIC_Citizens --#

summary(networkdata$METRIC_Citizens)

# (1) Voting_Method = Televote
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_Citizens > 0.1, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_Citizens > 0.1, 3])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_Citizens > 0.1 & networkdata$Points >= 10, 1])

# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_Citizens > 0.1, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_Citizens > 0.1, 3])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_Citizens > 0.1 & networkdata$Points >= 10, 1])

#-- METRIC_COBCit --#

summary(networkdata$METRIC_COBCit)
# (1) Voting_Method = Televote
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.15, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.15, 3])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.15 & networkdata$Points >= 10, 1])

# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.15, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.15, 3])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.15 & networkdata$Points >= 10, 1])

# Further subdivide the data into semi-finals and final
# (1) Voting_Method = Televote & Round = sf1
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.25 & networkdata$Round == "sf1", 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.25 & networkdata$Round == "sf1", 3])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$METRIC_COBCit > 0.25 & networkdata$Points >= 10 & networkdata$Round == "sf1", 1])
# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.25 & networkdata$Round == "sf1", 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.25 & networkdata$Round == "sf1", 3])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "J" & networkdata$METRIC_COBCit > 0.25 & networkdata$Points >= 10 & networkdata$Round == "sf1", 1])

#-- CAP_DIST_km --#

summary(networkdata$CAP_DIST_km)
# (1) Voting_Method = Televote
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$CAP_DIST_km < 500, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$CAP_DIST_km < 500, 9])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$CAP_DIST_km < 500 & networkdata$Points >= 10, 1])
# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$CAP_DIST_km < 1500, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$CAP_DIST_km < 1500, 9])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$CAP_DIST_km < 500 & networkdata$Points >= 10, 1])

#-- Average_Points --#

summary(networkdata$Average_Points)
# (1) Voting_Method = Televote
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "T" & networkdata$Average_Points > 8, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "T" & networkdata$Average_Points > 8, 9])
is_weighted(graph = G)
# interactive drag and place Social Network plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$Average_Points > 8 & networkdata$Points >= 10, 1])
# (2) Voting_Method = Jury
# Construct Social Network
G <- graph_from_data_frame(d = networkdata[networkdata$Voting_Method == "J" & networkdata$Average_Points > 8, 1:2], directed = T)
E(G)$weight <- as.numeric(networkdata[networkdata$Voting_Method == "J" & networkdata$Average_Points > 8, 9])
is_weighted(graph = G)
# interactive drag and place plot 
tkplot(G, edge.color = "grey", vertex.color = "orange", vertex.label.color = "black", vertex.size = 50, edge.arrow.size = 1, edge.label = E(G)$weight, edge.label.color = "black")
length(networkdata[networkdata$Voting_Method == "T" & networkdata$Average_Points > 8 & networkdata$Points >= 10, 1])


#######################################################################################################################
## SECTION 3 - DATA PROCESSING ########################################################################################
#######################################################################################################################

# In this script I shall process the ESC data for the data modelling stage
# This will incorporate the following:
# (1) Redefine variables as factor or numeric
# (2) Dividing the variables into the three predefined groups; 
# (i) Performance
# (ii) External 
# (iii) Competition
# (3) Normalise the Numeric Data to have mean 0 and standard deviation 1
# (4) Dummy Encoding all Categorical Factor levels.
# (5) Data Reduction
# (i) Redundant Variables
# (ii) Variables of Linear Combinations
# (iii) Categorical Variales via Chi-Squared Tests of Asscoiation

# It is also possible to:
# (1) Derive Interaction Terms
# (2) Derive Polynomial Terms
# However, it may be easier and more efficient to define these individually 
# during the model building stage.

# NOTE: With regards to citation
# Please see "Stats: Data and Models" by 
# (1) Richard D.De Veaux
# (2) Paul F.Velleman
# (3) David E.Bock
# for information related to:
# (i) data processing
# (ii) data modelling

#-- Libraries --#




#-- Load in the Data--#

# set the working directory
setwd(dir = "C:/Users/Margaret/Documents/Oisin/Education/MSc Computing Data Analytics/Semester 3/Data")
# load in the data
ESCdata <- read.csv(file = "NEWESC_2016_voting_data.csv", 
                    header = T)
# VERY IMPORTANT NOTE
# Make sure the Data is Ordered by To_country, From_country, Round and OOA
ESCdata <- ESCdata[order(ESCdata$To_country, 
                         ESCdata$From_country, 
                         ESCdata$Round, 
                         ESCdata$OOA),]

#####################################################################################
## Remove ID variable ###############################################################
#####################################################################################

ESCdata <- subset(x = ESCdata,
                  select = -ID)

#####################################################################################
## Redefine Variables ###############################################################
#####################################################################################

# Some of the numeric music features need to be redefined as nominal variables
# the variables are key, mode and time signature
ESCdata$key <- as.factor(ESCdata$key)
ESCdata$mode <- as.factor(ESCdata$mode)
ESCdata$time_signature <- as.factor(ESCdata$time_signature)
ESCdata$VBlocs1_FC <- as.factor(ESCdata$VBlocs1_FC)
ESCdata$VBlocs2_FC <- as.factor(ESCdata$VBlocs2_FC)
ESCdata$VBlocs1_TC <- as.factor(ESCdata$VBlocs1_TC)
ESCdata$VBlocs2_TC <- as.factor(ESCdata$VBlocs2_TC)

###################################################################################
## REMOVE MISSING OBSERVATIONS ####################################################
###################################################################################

# There are 1022 rows with missing data values
nrow(ESCdata) - nrow(ESCdata[complete.cases(ESCdata),])

# This accounts for just over 60% of the data
(nrow(ESCdata) - nrow(ESCdata[complete.cases(ESCdata),])) / nrow(ESCdata)

# filter our rows with missing data values
completedata <- ESCdata[complete.cases(ESCdata),]

# have 658 complete observations
nrow(completedata)

# NOTE: Observe how these missing observations effect the To_country and From_country
# Remember all the migration factors are intrinsically linked to to both
# From_country and two_country
TC_FC_missing_obseration_df <- as.data.frame(matrix(nrow = 42, ncol = 7))
colnames(TC_FC_missing_obseration_df) <- c("Country", "FC - Inc. Miss Obs.", "FC - Exc. Miss Obs.", 
                                           "FC - Miss Obs. %", "TC - Inc. Miss Obs.", 
                                           "TC - Exc. Miss Obs.", "TC - Miss Obsv. %")
# Use a for loop to fill in the data frame
for(i in 1:42){
  # Fill in the Country Column
  TC_FC_missing_obseration_df[i,1] <- levels(ESCdata$From_country)[i]
  # Fill in the FC-Inc. Column
  TC_FC_missing_obseration_df[i,2] <- summary(ESCdata$From_country)[[i]]
  # Fill in the FC-Exc. Column
  TC_FC_missing_obseration_df[i,3] <- summary(completedata$From_country)[[i]]
  # Fill in the FC-Missing Prct
  TC_FC_missing_obseration_df[i,4] <- round((1 - (summary(completedata$From_country)[[i]] / summary(ESCdata$From_country)[[i]])) * 100,
                                            digits = 2)
  # Fill in the TC-Inc. Column
  TC_FC_missing_obseration_df[i,5] <- summary(ESCdata$To_country)[[i]]
  # Fill in the TC-Exc. Column
  TC_FC_missing_obseration_df[i,6] <- summary(completedata$To_country)[[i]]
  # Fill in the TC-Missing Prct
  TC_FC_missing_obseration_df[i,7] <- round((1 - (summary(completedata$To_country)[[i]] / summary(ESCdata$To_country)[[i]])) * 100,
                                            digits = 2)
}                                      
# There is a substancial amount of data being lost here
# This is by far a major limitation in the research
# definitely a possible area of improvement for future research
# We could use an alternative source such as the world bank
# http://www.worldbank.org/en/topic/migrationremittancesdiasporaissues/brief/migration-remittances-data
# however these do not store the data based on country of birth / citizenship
# write the TC_FC_missing_observations_df to a csv file
# write.csv(x = TC_FC_missing_obseration_df,
#           file = "D.P._TC_FC_missing_obseration_df.csv")

# The Effect on To_country and From_country

# Bar Chart of To_country
plot1 <- ggplot(data = ESCdata, mapping = aes(x = To_country, fill = To_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of To_country", x = "To_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
plot2 <- ggplot(data = completedata, mapping = aes(x = To_country, fill = To_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of To_country", x = "To_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
grid.arrange(plot1, plot2)

# Bar Chart of From_country
plot1 <- ggplot(data = ESCdata, mapping = aes(x = From_country, fill = From_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of From_country", x = "From_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
plot2 <- ggplot(data = completedata, mapping = aes(x = From_country, fill = From_country)) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of From_country", x = "From_country", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
grid.arrange(plot1, plot2)


# Bar Chart of Points
plot1 <- ggplot(data = ESCdata, mapping = aes(x = as.factor(Points), fill = as.factor(Points))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Points", x = "Points", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
plot2 <- ggplot(data = completedata, mapping = aes(x = as.factor(Points), fill = as.factor(Points))) + 
  geom_bar(stat = "count", width = 0.7) + 
  labs(title = "Bar Chart of Points", x = "Points", y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
  guides(fill = FALSE)
grid.arrange(plot1, plot2)

# Using Germany as an example
subset(x = ESCdata[ESCdata$From_country == "Germany",],
       select = c(From_country, To_country, METRIC_COB, METRIC_Citizens, METRIC_COBCit))
# write to csv file
# write.csv(x = subset(x = ESCdata[ESCdata$From_country == "Germany",],
#                      select = c(From_country, To_country, METRIC_COB, METRIC_Citizens, METRIC_COBCit)),
#           file = "D.P._Germany_Case.csv")

###################################################################################
## DIVIDE THE DATA INTO FACTOR BLOCS ##############################################
###################################################################################

# Divide the Data into the Four variable Blocs;
# (1) voting_factors
voting_factors <- subset(x = completedata,
                         select = c(From_country, To_country, Points))
# There 3 variables in the voting factors bloc
ncol(voting_factors)

# (2) competition_factors
competition_factors <- subset(x = completedata,
                              select = c(Average_Points, Round, Voting_Method,
                                         Host_Nation, OOA))
# There are 5 variables in the competition factors bloc
ncol(competition_factors)

# (3) external_factors
external_factors <- subset(x = completedata,
                           select = c(VBlocs1_FC, VBlocs2_FC, 
                                      VBlocs1_TC, VBlocs2_TC, 
                                      ComVBlocs1, ComVBlocs2, 
                                      FC_LANGFAM, TC_LANGFAM, ComLANGFAM,
                                      Neighbours, TC_NumNeigh, FC_NonCOB,
                                      FC_NonCitzens, FC_COB, FC_Citizens,
                                      FC_Population, METRIC_COB, METRIC_Citizens,
                                      METRIC_COBCit, FC_GDP_mil, TC_GDP_mil,
                                      GDP_PROP, FC_CAP_LAT, FC_CAP_LON, 
                                      TC_CAP_LAT, TC_CAP_LON, CAP_DIST_km))
# There are 27 variables in the external factors bloc
ncol(external_factors)

# (4) performance_factors
performance_factors <- subset(x = completedata,
                              select = c(TC_PerfType, TC_SingerGender, FC_SONGLANG,
                                         TC_SONGLANG, ComSONGLAN, danceability, energy,
                                         key, loudiness, mode, speechiness, acousticness,
                                         instrumentalness, liveness, valence, tempo,
                                         duration_ms, time_signature))
# There are 18 variables in the performance factors bloc
ncol(performance_factors)

####################################################################################
## EXTRACT NUMERIC & CATEGORICAL FEATURES ##########################################
####################################################################################

# want to extract the numeric & categorical features for each variable bloc
# I shall define two functions that extract the numeric variables and
# the categoricalvariables seperately

#-- Function Definition --#

# (1) Function to extract numeric data

extract_numeric_data <- function(dataset) {
  # This function extracts the numeric attributes out of a dataset and stores them in a seperate dataset
  # dataset: the specified dataset to extract the numeric attributes from
  # create a data frame to hold the numeric attributes
  num_row <- nrow(dataset)
  num_col <- sum(sapply(X = dataset, FUN = function(x) is.numeric(x)))
  numeric_data <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
  # we will use a seperate index j to store the derived features in the power_data
  j = 1
  for (i in 1:ncol(dataset)){
    if(is.numeric(dataset[,i])) {
      numeric_data[,j] <- dataset[,i]
      colnames(numeric_data)[j] <- colnames(dataset)[i]
      j = j + 1
    }
  }
  return(numeric_data)
}

# (2) Function to extract factor data

extract_factor_data <- function(dataset) {
  # This function extracts the categorical attributes out of a dataset and stores them in a seperate dataset
  # dataset: the specified dataset to extract the categorical attributes from
  # create a data frame to hold the categorical attributes
  num_row <- nrow(dataset)
  num_col <- sum(sapply(X = dataset, FUN = function(x) is.factor(x)))
  factor_data <- as.data.frame(matrix(nrow = num_row, ncol = num_col))
  # we will use a seperate index j to store the derived features in the power_data
  j = 1
  for (i in 1:ncol(dataset)){
    if(is.factor(dataset[,i])) {
      factor_data[,j] <- dataset[,i]
      colnames(factor_data)[j] <- colnames(dataset)[i]
      j = j + 1
    }
  }
  return(factor_data)
}

#-- Competition Factors --#

# (i) Numeric Variables
competition_factors_numeric <- extract_numeric_data(dataset = competition_factors)
# There are 2 numeric competition factors
ncol(competition_factors_numeric)

# (ii) Categorical competition Variables
competition_factors_categorical <- extract_factor_data(dataset = competition_factors)
# There are 3 categorical variables 
ncol(competition_factors_categorical)

#-- External Factors --#

# (i) Numeric Variables
external_factors_numeric <- extract_numeric_data(dataset = external_factors)
# There are 17 numeric external variables
ncol(external_factors_numeric)

# (ii) Categorical Variables
external_factors_categorical <- extract_factor_data(dataset = external_factors)
# There are 13 categorical external variables
ncol(external_factors_categorical)

#-- Performance Factors --#

# (i) Numeric Variables
performance_factors_numeric <- extract_numeric_data(dataset = performance_factors)
# There are 10 numeric performance factors
ncol(performance_factors_numeric)

# (ii) Categorical Variables
performance_factors_categorical <- extract_factor_data(dataset = performance_factors)
# There are 8 categorical performance factors 
ncol(performance_factors_categorical)

#-- Voting Factors --#

# (i) Numeric Variables
voting_factors_numeric <- extract_numeric_data(dataset = voting_factors)
# This 1 numeric voting factor
ncol(voting_factors_numeric)

#  (ii) Categorical Variables
voting_factors_categorical <- extract_factor_data(dataset = voting_factors)
# There are 2 categorical voting factors
ncol(voting_factors_categorical)

#################################################################################
## DUMMY ENCODING FOR CATEGORICAL VARIABLES #####################################
#################################################################################

# Similar to above, I shall now dummy encode the categorical variables 
# for each variable bloc
# I shall first define the dummy encoding function
# afterwards I shall apply it to each categorical bloc and overwrite the input too

# NOTE: It is not neccessary to dummy encode the voting factors To_country and From_country
# as we will not be incorporating the into our analysis, as they are not appropriate

#-- Function Definition --#

categorical_dummy_encoding <- function(dataset){
  # we need to dummy encode the categorical variables
  library(dummy)
  # first subset categorical variables
  cvariables <- categories(x = dataset)
  # dummy encode the categorical variables
  dcvariables <- dummy(x = dataset, object = cvariables)
  # we are turning all categorical variables into numeric binary dummy variables
  # as such these numeric binary dummy variables cannot be defined as categorical variables
  # as regression functions cannot handle categorical variables
  # turn all variables into double precision numeric variables
  for (i in 1:length(dcvariables)) {
    dcvariables[,i] <- as.double(dcvariables[,i])
  }
  dcvariables[dcvariables == 1] <- 0
  dcvariables[dcvariables == 2] <- 1
  dcvariables <- as.data.frame(x = dcvariables)
  # subset continuous variables
  # use set operations
  nvariables <- dataset[,setdiff(x = attributes(dataset)$names, y = attributes(cvariables)$names)]
  dummy_encoded_data <- as.data.frame(cbind(nvariables, dcvariables))
  return(dummy_encoded_data)
}

#-- Competition Factors --#

competition_factors_categorical <- categorical_dummy_encoding(dataset = competition_factors_categorical)
# There are now 7 categorical competition factors
ncol(competition_factors_categorical)
# Prior to dummy encoding there were 3 categorical competition factors

#-- External Factors --#

external_factors_categorical <- categorical_dummy_encoding(dataset = external_factors_categorical)
# There are now 86 categorical external factors
ncol(external_factors_categorical)
# Prior to dummy encoding there were 13 categorical external factors
# This is a 753.85% increase
# As such a data reduction will be required prior to data modelling

#-- Performance Factors --#

performance_factors_categorical <- categorical_dummy_encoding(dataset = performance_factors_categorical)
# There are now 34 categorical eternal variables
ncol(performance_factors_categorical)
# Prior to dummy encoding there were 8 categorical variables
# This is a 475% increase
# As such a data reduction will be required prior to data modleling

##################################################################################
## STANDARDISE NUMERIC DATA ######################################################
##################################################################################

# In this section I shall normalise or range standardised 
# the numeric variables in each block
# There is also a special case for OOA in thecompetition Bloc
# OOA will need to be standardised in relation to each round
# I shall do this after I have standardised the numeric variables from each bloc

#-- Function Definition --#

# (1) RANGE STANDARDISATION TO [0,1]
range_standardise_data <- function(dataset, lower_bound = 0, upper_bound = 1) {
  standardised_data <- dataset
  # dataset: the specified dataset to range standardised
  # the inner function defines the range standardisation transform, as R does not have one
  # the outer function applies range standardisation to the numeric variables of the specified dataset
  range_normalisation <- function(dataset, lb, ub) {
    # range standardisation [lower bound, upper_bound], e.g. [0, 1]
    # Convert data to have minimum 0 and maximum 1
    mx <- max(dataset)
    mn <- min(dataset)
    ((((dataset - mn) / (mx - mn)) * (ub - lb)) + lb)
  }
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- range_normalisation(dataset = dataset[,i], lb = lower_bound, ub = upper_bound)
    }
  }
  return(standardised_data)
}

# (2) DATA NORMALISATION TO MEAN 0 AND STANDARD DEVIATION 1
normalise_data <- function(dataset) {
  standardised_data <- dataset
  # dataset: the specified dataset to range standardised
  # the inner function defines the range standardisation transform, as R does not have one
  # the outer function applies range standardisation to the numeric variables of the specified dataset
  data_normalisation <- function(dataset) {
    # range standardisation [lower bound, upper_bound], e.g. [0, 1]
    # Convert data to have minimum 0 and maximum 1
    mean <- mean(dataset)
    standard.deviation <- sd(dataset)
    ((dataset - mean) / standard.deviation)
  }
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- data_normalisation(dataset = dataset[,i])
    }
  }
  return(standardised_data)
}

#-- Standardise the Data --#

# (1) Competition Factors
# competition_factors_numeric <- range_standardise_data(dataset = competition_factors_numeric, lower_bound = 0, upper_bound = 1)
competition_factors_numeric <- normalise_data(dataset = competition_factors_numeric)
# Check that each column has mean 0
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = mean), 
      digits = 6)
# Check that each column has standard deviation 1
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = sd), 
      digits = 6)


# (2) External Factors
# external_factors_numeric <- range_standardise_data(dataset = external_factors_numeric, lower_bound = 0, upper_bound = 1)
external_factors_numeric <- normalise_data(dataset = external_factors_numeric)
# Check that each column has mean 0
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = mean), 
      digits = 6)
# Check that each column has standard deviation 1
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = sd), 
      digits = 6)

# (3) Performance Factors
# performance_factors_numeric <- range_standardise_data(dataset = performance_factors_numeric, lower_bound = 0, upper_bound = 1)
performance_factors_numeric <- normalise_data(dataset = performance_factors_numeric)
# Check that each column has mean 0
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = mean), 
      digits = 6)
# Check that each column has standard deviation 1
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = sd), 
      digits = 6)

# (4) Voting Factors
# voting_factors_numeric <- range_standardise_data(dataset = voting_factors_numeric, lower_bound = 0, upper_bound = 1)
# voting_factors_numeric <- normalise_data(dataset = voting_factors_numeric)

# NOTE: It is not necessary to standardise the response variable score.
# I will most likely be transforming score to satisfy the normality assumptions of MLR

#-- OOA --#

# Extract the relevent features
OOA_df <- subset(x = completedata,
                 select = c(From_country, To_country, Round, OOA))
# Order the Features
OOA_df <- OOA_df[order(OOA_df$Round, OOA_df$OOA, OOA_df$To_country, OOA_df$To_country),] 
head(OOA_df)
# (1) Define the range standardisation
range_normalisation <- function(dataset, lb, ub) {
  # range standardisation [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mx <- max(dataset)
  mn <- min(dataset)
  ((((dataset - mn) / (mx - mn)) * (ub - lb)) + lb)
}
# (2) Define the data normalisation
data_normalisation <- function(dataset) {
  # range standardisation [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mean <- mean(dataset)
  standard.deviation <- sd(dataset)
  ((dataset - mean) / standard.deviation)
}
# Apply the range standardisation
OOA_df[OOA_df$Round == "f", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "f", 4], lb = 0, ub = 1)
OOA_df[OOA_df$Round == "sf1", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "sf1", 4], lb = 0, ub = 1)
OOA_df[OOA_df$Round == "sf2", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "sf2", 4], lb = 0, ub = 1)
head(OOA_df)
# Apply the data normalisation
# OOA_df[OOA_df$Round == "f", 4] <- data_normalisation(dataset = OOA_df[OOA_df$Round == "f", 4])
# OOA_df[OOA_df$Round == "sf1", 4] <- data_normalisation(dataset = OOA_df[OOA_df$Round == "sf1", 4])
# OOA_df[OOA_df$Round == "sf2", 4] <- data_normalisation(dataset = OOA_df[OOA_df$Round == "sf2", 4])
# head(OOA_df)
# Re-Order the dataframe
OOA_df <- OOA_df[order(OOA_df$To_country, OOA_df$From_country, OOA_df$Round, OOA_df$OOA),]
head(OOA_df)
head(subset(x = completedata,
            select = c(From_country, To_country, Round, OOA)))
# Overwrite the OOA in competition_factors_numeric with the new standardised OOA
head(competition_factors_numeric)
competition_factors_numeric[,2] <- OOA_df[,4]

##################################################################################
## Data Reducation ###############################################################
##################################################################################

# I shall perform a data reduction in this section
# Here I shall remove categorcal variables which:
# (1) Have only a single type of observation, 0 or 1
# (2) Variables that form part of a linear combination
# (3) Categorical Varibles that are strongly Associated
# (4) Numeric Variables that are trongly Correlated

#######################################################
## Remove Categorical Variables with no Observations ##
#######################################################

# Here I shall remove categorical variables which have only 0 observations

#-- External Factors --#

# There are 22 categorical varables that have only 0 observations
sum(apply(X = external_factors_categorical, 
          MARGIN = 2,
          FUN = sum) == 0)

# The following external categorical varables have only 0 observations
# (1) VBlocs1_FC_G
# (2) VBlocs1_TC_U
# (3) FC_LANGFAM_Albanian
# (4) FC_LANGFAM_Armenian
# (5) FC_LANGFAM_Hellenic
# (6) FC_LANGFAM_Kartvelian
# (7) FC_LANGFAM_Semetic
# (8) FC_LANGFAM_Semitic
# (9) FC_LANGFAM_Turkic
# (10) TC_LANGFAM_Albanian
# (11) TC_LANGFAM_Kartvelian
apply(X = external_factors_categorical, 
      MARGIN = 2,
      FUN = sum) == 0

external_factors_categorical <- subset(x = external_factors_categorical,
                                       select = -c(VBlocs1_FC_3, VBlocs1_FC_4, VBlocs1_FC_6,
                                                   VBlocs1_FC_9, VBlocs1_FC_11, VBlocs1_FC_12,
                                                   VBlocs1_FC_13, VBlocs1_FC_14, VBlocs1_FC_19,
                                                   VBlocs1_FC_20, VBlocs1_TC_12, VBlocs1_TC_14,
                                                   VBlocs1_TC_20, FC_LANGFAM_Albanian, FC_LANGFAM_Armenian,
                                                   FC_LANGFAM_Hellenic, FC_LANGFAM_Kartvelian, FC_LANGFAM_Semetic, 
                                                   FC_LANGFAM_Semitic, FC_LANGFAM_Turkic, TC_LANGFAM_Albanian,
                                                   TC_LANGFAM_Kartvelian))

#-- Competition Factors --#

# There are no competition categorical variables with only 0 observations
sum(apply(X = competition_factors_categorical, 
          MARGIN = 2,
          FUN = sum) == 0)

#-- Performance Factors--#

# There are 2 performance categorical variables with only 0 observations
sum(apply(X = performance_factors_categorical, 
          MARGIN = 2,
          FUN = sum) == 0)

# The following are the performance categorical variables with only 0 observations
# (1) FC_SONGLANG_Bosnian
# (2) FC_SONGLANG_Macedonian
apply(X = performance_factors_categorical, 
      MARGIN = 2,
      FUN = sum) == 0

performance_factors_categorical <- subset(x = performance_factors_categorical,
                                          select = -c(FC_SONGLANG_Bosnian, FC_SONGLANG_Macedonian))

######################################
## Variables of Linear Combinations ##
######################################

# Remove all categorical variables that are the binary opposites
# (1) Performance variables
performance_factors_categorical <- subset(performance_factors_categorical, 
                                          select = -c(mode_0,
                                                      time_signature_3))

# (2) competition variables
competition_factors_categorical <- subset(competition_factors_categorical,
                                          select = -c(Host_Nation_n))

# (3) external variables
external_factors_categorical <- subset(external_factors_categorical,
                                       select = -c(ComVBlocs1_n, ComVBlocs2_n,
                                                   ComLANGFAM_n,
                                                   Neighbours_n))

# NOTE: also remove variables which are a linear combination of other varibles
# (1) Competition Variables
competition_factors_categorical <- subset(competition_factors_categorical,
                                          select = -c(Round_sf2, Voting_Method_T))

# (2) External Variables
performance_factors_categorical <- subset(performance_factors_categorical,
                                          select = -c(TC_PerfType_Duet))

###################################################################
## Remove Unncessary Categorical Variables via Chi-squared Tests ##
###################################################################

# If two variables are feature a lot of 0s or 1s 
# then there will be a strong association between the two variables
# as they share a lot of common observations
# thus one of the variables can be removed as they both measure the same entity
# this lowers the chance of collinearity and reduces the number of dimensions

# First lets screen how many observations exist in each variable
apply(X = external_factors_categorical, 
      MARGIN = 2,
      FUN = sum)

#-- Function Definition --#

# Define a Function to preform chi-squared tests and store the results as a data frame
chisq_tests <- function(dataset) {
  # FUNCTION OVERVIEW
  # this function takes in a dataset of purely categroical variables
  # and applies a chi-squared test of association between each one
  # There are two parts to it
  # (1) Create the data frameto store the chi-squared tests
  # (2) Fill the data frame with the relevant information
  # The underlying concept is that is valade to remove a variable if
  # (1) It is highly associated with another variable
  # (2) It is sparse
  #-- PART 1
  # First create a dataframe to store the relevent chi-squared test data
  data_frame_rows <- ncol(dataset)
  chisqtestdf <- as.data.frame(matrix(nrow = (data_frame_rows)^2, 
                                      ncol = 5))
  # rename the columns of the data frame
  colnames(chisqtestdf) <- c("X", "Y", "X-Obs", "Y-Obs", "P-Value")
  #-- PART 2
  # r represents the row index and will be used to input the relevent data
  r = 1
  for (i in 1:data_frame_rows) {
    for (j in 1:data_frame_rows) {
      # Save the variables name being tested
      chisqtestdf[r,1] <- colnames(dataset)[i]
      chisqtestdf[r,2] <- colnames(dataset)[j]
      # Input the number of observations
      chisqtestdf[r,3] <- apply(X = dataset, 
                                MARGIN = 2,
                                FUN = sum)[i]
      chisqtestdf[r,4] <- apply(X = dataset, 
                                MARGIN = 2,
                                FUN = sum)[j]
      # Conduct the chi-squared test and savethe p-value
      chisqtestdf[r,5] <- round(x = chisq.test(x = as.factor(dataset[,i]),
                                               y = as.factor(dataset[,j]))$p.value,
                                digits = 5)
      r = r + 1
    }
  }
  # Set the output of the function to be the chi-squared test dataframe
  return(chisqtestdf)
}

#-- Voting Blocs --#

# (1) FC_VBlocs1
# Subset the data to be tested
FC_VBlocs_1df <- subset(x = external_factors_categorical, 
                        select = c(VBlocs1_FC_1, VBlocs1_FC_2, VBlocs1_FC_5,
                                   VBlocs1_FC_7, VBlocs1_FC_8, 
                                   VBlocs1_FC_10,
                                   VBlocs1_FC_15, VBlocs1_FC_17,
                                   VBlocs1_FC_21))
# Run the Chi-Squared Tests
chisq_tests_FC_VBlocs1 <- chisq_tests(dataset = FC_VBlocs_1df)
# Filter out the significant p-values
chisq_tests_FC_VBlocs1[chisq_tests_FC_VBlocs1$'P-Value' < 0.05 & chisq_tests_FC_VBlocs1$`X-Obs` < 66,]

# (2) FC_VBlocs2
# Subset the data to be tested
FC_VBlocs_2df <- subset(x = external_factors_categorical, 
                        select = c(VBlocs2_FC_1, VBlocs2_FC_2, VBlocs2_FC_3, VBlocs2_FC_4, VBlocs2_FC_5,
                                   VBlocs2_FC_6))
# Run the Chi-Squared Tests
chisq_tests_FC_VBlocs2 <- chisq_tests(dataset = FC_VBlocs_2df)
# Filter out the significant p-values
chisq_tests_FC_VBlocs2[chisq_tests_FC_VBlocs2$'P-Value' < 0.05 & chisq_tests_FC_VBlocs2$`X-Obs` < 66,]

# (3) TC_VBlocs1
# Subset the data to be tested
TC_VBlocs_1df <- subset(x = external_factors_categorical, 
                        select = c(VBlocs1_TC_1, VBlocs1_TC_2, VBlocs1_TC_3, VBlocs1_TC_4, VBlocs1_TC_5,
                                   VBlocs1_TC_6, VBlocs1_TC_7, VBlocs1_TC_8, VBlocs1_TC_9, VBlocs1_TC_10,
                                   VBlocs1_TC_11, VBlocs1_TC_13, VBlocs1_TC_15,
                                   VBlocs1_TC_16, VBlocs1_TC_17, VBlocs1_TC_18, VBlocs1_TC_19,
                                   VBlocs1_TC_21))
# Run the Chi-Squared Tests
chisq_tests_TC_VBlocs1 <- chisq_tests(dataset = TC_VBlocs_1df)
# Filter out the significant p-values
chisq_tests_TC_VBlocs1[chisq_tests_TC_VBlocs1$'P-Value' < 0.05 & chisq_tests_TC_VBlocs1$`X-Obs` < 66,]

# (4) TC_VBlocs2
# Subset the data to be tested
TC_VBlocs_2df <- subset(x = external_factors_categorical, 
                        select = c(VBlocs2_TC_1, VBlocs2_TC_2, VBlocs2_TC_3, VBlocs2_TC_4, VBlocs2_TC_5,
                                   VBlocs2_TC_6))
# Run the Chi-Squared Tests
chisq_tests_TC_VBlocs2 <- chisq_tests(dataset = TC_VBlocs_2df)
# Filter out the significant p-values
chisq_tests_TC_VBlocs2[chisq_tests_TC_VBlocs2$'P-Value' < 0.05 & chisq_tests_TC_VBlocs2$`X-Obs` < 66,]

# Remove Unneccessary Categorical Variables 
external_factors_categorical <- subset(external_factors_categorical,
                                       select = -c(VBlocs1_FC_2, VBlocs1_FC_5, VBlocs1_FC_7, VBlocs1_FC_8,
                                                   VBlocs1_FC_10, VBlocs1_FC_15, VBlocs1_FC_17, VBlocs1_FC_21,
                                                   VBlocs2_FC_2,
                                                   VBlocs1_TC_2, VBlocs1_TC_4, VBlocs1_TC_5,
                                                   VBlocs1_TC_6, VBlocs1_TC_7, VBlocs1_TC_10, VBlocs1_TC_11,
                                                   VBlocs1_TC_15, VBlocs1_TC_16, VBlocs1_TC_17, VBlocs1_TC_18, 
                                                   VBlocs1_TC_19, VBlocs1_TC_21))

# combine the datasets and write to a .csv file
# chisq_tests_TC_VBlocs <- rbind(chisq_tests_FC_VBlocs1[chisq_tests_FC_VBlocs1$'P-Value' < 0.05 & chisq_tests_FC_VBlocs1$`X-Obs` < 66,],
#                                chisq_tests_FC_VBlocs2[chisq_tests_FC_VBlocs2$'P-Value' < 0.05 & chisq_tests_FC_VBlocs2$`X-Obs` < 66,],
#                                chisq_tests_TC_VBlocs1[chisq_tests_TC_VBlocs1$'P-Value' < 0.05 & chisq_tests_TC_VBlocs1$`X-Obs` < 66,],
#                                chisq_tests_TC_VBlocs2[chisq_tests_TC_VBlocs2$'P-Value' < 0.05 & chisq_tests_TC_VBlocs2$`X-Obs` < 66,])
# write.csv(x = chisq_tests_TC_VBlocs,
#            file = "Exploratory Analysis/Descriptive Statistics/E.A._chisq_tests_VBlocs.csv",
#            row.names = F)

#-- Language Family --#

# Subset the data to be tested
# (1)FC_LANGFAM
FC_LANGFAM_df <- subset(x = external_factors_categorical, 
                        select = c(FC_LANGFAM_Baltic, FC_LANGFAM_Germanic,
                                   FC_LANGFAM_ItalicRomance,
                                   FC_LANGFAM_Slavic, FC_LANGFAM_Uralic))
# (2) TC_LANGFAM
TC_LANGFAM_df <- subset(x = external_factors_categorical, 
                        select = c(TC_LANGFAM_Armenian,
                                   TC_LANGFAM_Baltic, TC_LANGFAM_Germanic,
                                   TC_LANGFAM_Hellenic, TC_LANGFAM_ItalicRomance,
                                   TC_LANGFAM_Semetic,
                                   TC_LANGFAM_Semitic, TC_LANGFAM_Slavic,
                                   TC_LANGFAM_Turkic, TC_LANGFAM_Uralic))
# Run the Chi-Squared Tests
chisq_tests_FC_LANGFAM_df <- chisq_tests(dataset = FC_LANGFAM_df)
chisq_tests_TC_LANGFAM_df <- chisq_tests(dataset = TC_LANGFAM_df)
# Filter out the significant p-values
chisq_tests_FC_LANGFAM_df[chisq_tests_FC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_FC_LANGFAM_df$`X-Obs` < 66,]
chisq_tests_TC_LANGFAM_df[chisq_tests_TC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_TC_LANGFAM_df$`X-Obs` < 66,]
# combine the datasets and write to a .csv file
# chisq_tests_LANGFAM <- rbind(chisq_tests_FC_LANGFAM_df[chisq_tests_FC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_FC_LANGFAM_df$`X-Obs` < 66,],
#                                chisq_tests_TC_LANGFAM_df[chisq_tests_TC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_TC_LANGFAM_df$`X-Obs` < 66,])
# write.csv(x = chisq_tests_LANGFAM,
#           file = "Exploratory Analysis/Descriptive Statistics/E.A._chisq_tests_LANGFAM.csv",
#           row.names = F)
# Remove Unneccessary Categorical Variables 
external_factors_categorical <- subset(x = external_factors_categorical,
                                       select = -c(FC_LANGFAM_Baltic, FC_LANGFAM_ItalicRomance,
                                                   TC_LANGFAM_ItalicRomance, TC_LANGFAM_Armenian,
                                                   TC_LANGFAM_Baltic, TC_LANGFAM_Hellenic,
                                                   TC_LANGFAM_Semetic, TC_LANGFAM_Semitic,
                                                   TC_LANGFAM_Turkic, TC_LANGFAM_Uralic))

#-- Song Language --#

# Subset the data to be tested
# (1) FC_SONGLANG
FC_SONGLANG_df <- subset(x = performance_factors_categorical, 
                         select = c(FC_SONGLANG_English, FC_SONGLANG_French,
                                    FC_SONGLANG_Mixed))
# (2) TC_SONGLANG
TC_SONGLANG_df <- subset(x = performance_factors_categorical, 
                         select = c(TC_SONGLANG_Bosnian, TC_SONGLANG_English, 
                                    TC_SONGLANG_French, TC_SONGLANG_Macedonian,
                                    TC_SONGLANG_Mixed))
# Run the Chi-Squared Tests
chisq_tests_FC_SONGLANG_df <- chisq_tests(dataset = FC_SONGLANG_df)
chisq_tests_TC_SONGLANG_df <- chisq_tests(dataset = TC_SONGLANG_df)
# Filter out the significant p-values
chisq_tests_FC_SONGLANG_df[chisq_tests_FC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_FC_SONGLANG_df$`X-Obs` < 66,]
chisq_tests_TC_SONGLANG_df[chisq_tests_TC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_TC_SONGLANG_df$`X-Obs` < 66,]
# combine the datasets and write to a .csv file
# chisq_tests_FC_SONGLANG <- rbind(chisq_tests_FC_SONGLANG_df[chisq_tests_FC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_FC_SONGLANG_df$`X-Obs` < 66,],
#                              chisq_tests_TC_SONGLANG_df[chisq_tests_TC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_TC_SONGLANG_df$`X-Obs` < 66,])
# write.csv(x = chisq_tests_FC_SONGLANG,
#           file = "Exploratory Analysis/Descriptive Statistics/E.A._chisq_tests_FC_SONGLANG.csv",
#           row.names = F)
# Remove Unneccessary Categorical Variables 
performance_factors_categorical <- subset(x = performance_factors_categorical,
                                          select = -c(FC_SONGLANG_French, FC_SONGLANG_Mixed,
                                                      TC_SONGLANG_Bosnian, TC_SONGLANG_French,
                                                      TC_SONGLANG_Macedonian))

#-- Keys --#

# Subset the data to be tested
keys_df <- subset(x = performance_factors_categorical, 
                  select = c(key_0, key_1, key_2, key_3, key_4, key_5, key_6,
                             key_7, key_8, key_9, key_10, key_11))
# Run the Chi-Squared Tests
chisq_tests_keys_df <- chisq_tests(dataset = keys_df)
# Filter out the significant p-values
chisq_tests_keys_df[chisq_tests_keys_df$'P-Value' < 0.05 & chisq_tests_keys_df$`X-Obs` < 66,]
# combine the datasets and write to a .csv file
# chisq_tests_keys <- rbind(chisq_tests_keys_df[chisq_tests_keys_df$'P-Value' < 0.05 & chisq_tests_keys_df$`X-Obs` < 66,])
# write.csv(x = chisq_tests_keys,
#           file = "Exploratory Analysis/Descriptive Statistics/E.A._chisq_tests_keys.csv",
#          row.names = F)
# Remove Unneccessary Categorical Variables 
performance_factors_categorical <- subset(x = performance_factors_categorical,
                                          select = -c(key_0, key_1, key_9, key_10))

###################################################################
## Remove Unncessary Categorical Variables via Correlation Tests ##
###################################################################

# If two numeric variables are very highly correlated
# and represent the same entity
# then it is unncessary to include them in the data modelling stage
# This is particular the case for the migration data

# NOTE: I shall impelement the same correlation test function
# that was used during the exploratory analysis section

#-- Function Definition --#

correlation_tests <- function (dataset) {
  # Create a data frame to hold the correlation test data
  data_frame_rows <- ncol(dataset)^2
  cor_test_df <- as.data.frame(matrix(nrow = data_frame_rows, ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:ncol(dataset)) {
    for (j in 1:ncol(dataset)) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,i],
                       y = dataset[,j],
                       na.action = "na.omit")
      # Fill in the X Variable Name
      cor_test_df[r, 1] <- colnames(dataset)[i]
      # Fill in the Y Variable Name
      cor_test_df[r, 2] <- colnames(dataset)[j]
      # Fill in the correlation
      cor_test_df[r, 3] <- round(c.t.$estimate, digits = 5)
      # Fill in the p-value
      cor_test_df[r, 4] <- round(c.t.$p.value, digits = 5)
      # Update the row index
      r = r + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}

#-- Migration Data --#

# Perform the correlation tests
c.t. <- correlation_tests(dataset = subset(x = external_factors_numeric,
                                           select = c(FC_NonCOB, FC_NonCitzens, FC_COB,
                                                      FC_Citizens, FC_Population, METRIC_COB,
                                                      METRIC_Citizens, METRIC_COBCit)))
# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.9 & c.t.$Correlation < 1,]
# The following migration variables are very hevily correlated
# (1) FC_COB & FC_Citizens
# (2) FC_COB & FC_Population
# (3) METRIC_COBCit & METRIC_COB
# (4) METRIC_COBCit & METRIC_Citizens
# Thus we shall remove
# (1) FC_COB
# (2) METRIC_COBCit

# Remove the unneccessary variables
external_factors_numeric <- subset(x = external_factors_numeric,
                                   select = -c(FC_COB, FC_Citizens, FC_Population, METRIC_COB, METRIC_COBCit))

#################################################################################
## FINAL PROCESSED DATASET ######################################################
#################################################################################

# The Voting Factors
processed_voting_factors <-as.data.frame(cbind(voting_factors_categorical,
                                               voting_factors_numeric))

# The Competition Factors
processed_competition_factors <-as.data.frame(cbind(competition_factors_categorical,
                                                    competition_factors_numeric))

# The External Factors
processed_external_factors <- as.data.frame(cbind(external_factors_categorical,
                                                  external_factors_numeric))

# The Performance Factors
processed_performance_factors <- as.data.frame(cbind(performance_factors_categorical,
                                                     performance_factors_numeric))

# The final Processed Data Frame
processed_data <- as.data.frame(cbind(processed_voting_factors,
                                      processed_competition_factors,
                                      processed_external_factors,
                                      processed_performance_factors))

# Split the Datasets into the Televote and Jury datasets
televote_data <- processed_data[processed_data$Voting_Method_J == 0,]
jury_data <- processed_data[processed_data$Voting_Method_J == 1,]

#################################################################################
## Remove Unnecessary Data Frames from R ########################################
#################################################################################

# As there is a lot of unnecessary data frames saved to R
# I shall erasethem from R's memory using rm()

rm(competition_factors_categorical, competition_factors_numeric,
   external_factors_categorical, external_factors_numeric,
   performance_factors_categorical, performance_factors_numeric,
   voting_factors_categorical, voting_factors_numeric, OOA_df,
   chisq_tests_FC_LANGFAM_df, chisq_tests_FC_SONGLANG_df, chisq_tests_FC_VBlocs1,
   chisq_tests_FC_VBlocs2, chisq_tests_keys_df, chisq_tests_TC_LANGFAM_df,
   chisq_tests_TC_SONGLANG_df, chisq_tests_TC_VBlocs1, chisq_tests_TC_VBlocs2, 
   FC_LANGFAM_df, FC_SONGLANG_df,
   FC_VBlocs_1df, FC_VBlocs_2df, keys_df, 
   TC_LANGFAM_df, TC_SONGLANG_df, TC_VBlocs_1df, TC_VBlocs_2df,
   c.t., competition_factors, external_factors,
   performance_factors, TC_FC_missing_obseration_df, voting_factors,
   i, plot1, plot2)


#####################################################################################
## Final Output #####################################################################
#####################################################################################

# The final output from this scipt includes:
# (1) The Original ESCdata 
# (2) The Original ESCdata divided up into the 4 variable blocs
# (3) The 4 variable blocs processed
# (4) The entire Processed data as a data frame

#######################################################################################################################
## Data Modelling OVERVIEW ############################################################################################
#######################################################################################################################

library(car)

# A stratified analysis method will be implamented
# (1) Test Overall Data
# Create baseline model based on research
# Create my own model, iteratively
# Compare
# (2) Split Data by Voting Method
# Due to a lack of data I can't stratify the data by each country
# Need a minimum of 10/20 observations per covariate for regression analysis
# Split data by voting method, research televote shows more bias than the jury
# NOTE forward and stepwise fitting will be utilised using AIC to determine model of best fit

# NOTE: This script is a direct continuation on from the ESC_Data_Processing_II script

# NOTE: the models will be evaluated using the car package
# MLR requires the residuals to be ~ IID N(0, sigma^2)
# the residuals will be standardised for the accessment
# Normality Assumptions will be accessed using:
# (i) Normality tests from the nortest package
# (ii) Visualisations such as histograms, QQ-plots, Residual Plots and Add Variable Plots
# Constant Variance will be accessed using:
# (i) non-constand variance test
# Multicollinearity will be accessed using:
# (i) variance inflation factors
# Outliers will be accessed using:
# (i) Cooks Distance

######################################################################################################################
## My Model - Overall Data ###########################################################################################
######################################################################################################################

# Iteratively create my own mode; 
# continusously noting the magnitude and significance of the predictors. 
# Fit the variables in groups/blocs; 
# performance, competition and external variables. 
# Fit each variable block independently. 
# Then fit the all the best variables. 
# Incorporate interaction and squared terms. 
# Compare my final model with the baseline mode; 
# making notes of any differences/similarities

#################################
## Fitting Competition Factors ##
#################################

# Competition factors are the factors related to how the competition 
# There are 5 factors in this bloc
# (1) Average Points - The historical average number of points exchange between countries
# (2) Round - The Competition Round (sf1, f2 or f)
# (3) Voting_Method - The Voting Method (Televote or Jury)
# (4) Host_Nation - The Host Nation of the competition (Sweden)
# (5) Order of Appearance - the order of appearance for each participant

# fit the model
# stepwise 
min_model <- lm(Points ~ 1, data = processed_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + Round_f + Round_sf1 +
                          Voting_Method_J + Host_Nation_y + OOA,
                        data = processed_data))
# my_model_overall <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_overall <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_overall)
anova(my_model_overall)


#########################
## Performance Factors ##
#########################

# stepwise 
min_model <- lm(Points ~ 1, data = processed_data[,-c(1,2)])
max_model <- formula(lm(Points ~ TC_PerfType_Group  + TC_PerfType_Solo +
                          TC_SingerGender_Female + TC_SingerGender_Male +
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN_n +
                          key_2 + key_3 + key_4 + key_5 + key_6 + key_7 + key_8 + key_11 + mode_1 +
                          time_signature_4 + danceability + energy + loudiness + speechiness +
                          acousticness + instrumentalness + liveness + valence + tempo + duration_ms,
                        data = processed_data))
# my_model_overall <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_overall <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_overall)
anova(my_model_overall)
vif(my_model_overall)

######################
## External Factors ##
######################

# Fit Stepwise Model
min_model <- lm(Points ~ 1, data = processed_data[,-c(1,2)])
max_model <- formula(lm(Points ~ VBlocs1_FC_1 + VBlocs1_FC_18 +
                          VBlocs2_FC_1 + VBlocs2_FC_3 + VBlocs2_FC_4 + 
                          VBlocs2_FC_5 + VBlocs2_FC_6 + VBlocs1_TC_1 + 
                          VBlocs1_TC_3 + VBlocs1_TC_8 + VBlocs1_TC_9 +
                          VBlocs1_TC_13 + VBlocs2_TC_1 + VBlocs2_TC_2 + 
                          VBlocs2_TC_3 + VBlocs2_TC_4 + VBlocs2_TC_5 +
                          VBlocs2_TC_6 + ComVBlocs1_y + ComVBlocs2_y +
                          FC_LANGFAM_Germanic + FC_LANGFAM_ItalianRomance +
                          FC_LANGFAM_Slavic + FC_LANGFAM_Uralic + 
                          TC_LANGFAM_Germanic + TC_LANGFAM_Slavic +
                          ComLANGFAM_y + Neighbours_y + TC_NumNeigh + FC_NonCOB +
                          FC_NonCitzens + METRIC_Citizens + CAP_DIST_km,
                        data = processed_data))
# my_model_overall <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_overall <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_overall)
vif(my_model_overall)

#################
## All Factors ##
#################

# stepwise 
min_model <- lm(Points ~ 1, data = processed_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + OOA + speechiness + TC_PerfType_Solo +
                          key_7 + ComSONGLAN_n + FC_SONGLANG_English + key_5 + key_2 + 
                          key_6 + liveness + key_3 + time_signature_4 + 
                          METRIC_Citizens + VBlocs1_TC_3 + ComLANGFAM_y + FC_NonCitzens +
                          CAP_DIST_km + VBlocs1_TC_13 + ComVBlocs1_y + VBlocs1_TC_1,
                        data = processed_data))
# my_model_overall <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_overall <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_overall)
vif(my_model_overall)

#################
## Final Model ##
#################

my_model_overall <- lm(formula = Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                         ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])

summary(my_model_overall)
vif(my_model_overall)
anova(my_model_overall)

######################################################################################################################
## My Model - Split by Televote ######################################################################################
######################################################################################################################

# Consider how the televoting data differs from the jury data
# with regards to:
# performance features
# competition features
# external features

#########################
## Competition Factors ##
#########################

#-- All Competition Factors --#

# stepwise 
min_model <- lm(Points ~ 1, data = televote_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + Round_f + Round_sf1 +
                          Voting_Method_J + Host_Nation_y + OOA,
                        data = televote_data))
# my_model_tele <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_tele <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_tele)

##########################
## Performance Features ##
##########################

# stepwise 
min_model <- lm(Points ~ 1, data = televote_data[,-c(1,2)])
max_model <- formula(lm(Points ~ TC_PerfType_Group  + TC_PerfType_Solo +
                          TC_SingerGender_Female + TC_SingerGender_Male +
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN_n +
                          key_2 + key_3 + key_4 + key_5 + key_6 + key_7 + key_8 + key_11 + mode_1 +
                          time_signature_4 + danceability + energy + loudiness + speechiness +
                          acousticness + instrumentalness + liveness + valence + tempo + duration_ms,
                        data = televote_data))
# my_model_tele <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_tele <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_tele)
anova(my_model_tele)
vif(my_model_tele)

######################
## External Factors ##
######################

# Fit Stepwise Model
min_model <- lm(Points ~ 1, data = televote_data[,-c(1,2)])
max_model <- formula(lm(formula = Points ~ VBlocs1_FC_1 + VBlocs1_FC_18 +
                          VBlocs2_FC_1 + VBlocs2_FC_3 + VBlocs2_FC_4 + 
                          VBlocs2_FC_5 + VBlocs2_FC_6 + VBlocs1_TC_1 + 
                          VBlocs1_TC_3 + VBlocs1_TC_8 + VBlocs1_TC_9 +
                          VBlocs1_TC_13 + VBlocs2_TC_1 + VBlocs2_TC_2 + 
                          VBlocs2_TC_3 + VBlocs2_TC_4 + VBlocs2_TC_5 +
                          VBlocs2_TC_6 + ComVBlocs1_y + ComVBlocs2_y +
                          FC_LANGFAM_Germanic + FC_LANGFAM_ItalianRomance +
                          FC_LANGFAM_Slavic + FC_LANGFAM_Uralic + 
                          TC_LANGFAM_Germanic + TC_LANGFAM_Slavic +
                          ComLANGFAM_y + Neighbours_y + TC_NumNeigh + FC_NonCOB +
                          FC_NonCitzens + METRIC_Citizens + CAP_DIST_km,
                        data = televote_data))
# my_model_tele <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_tele <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_tele)
vif(my_model_tele)

#################
## All Factors ##
#################

# stepwise 
min_model <- lm(Points ~ 1, data = televote_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + OOA +
                          key_11 + mode_1 + acousticness + key_7 + TC_SONGLANG_Mixed +
                          instrumentalness + danceability + METRIC_Citizens +
                          VBlocs1_TC_3 + ComLANGFAM_y + VBlocs1_TC_13 + VBlocs2_TC_1,
                        data = televote_data))
# my_model_tele <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_tele <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_tele)

#################
## Final Model ##
#################

my_model_tele <- lm(formula = Points ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
summary(my_model_tele)
vif(my_model_tele)
anova(my_model_tele)

#######################################################################################################################
## My Model - Split by Jury ###########################################################################################
#######################################################################################################################

#########################
## Competition Factors ##
#########################

# stepwise 
min_model <- lm(Points ~ 1, data = jury_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + Round_f + Round_sf1 +
                          Voting_Method_J + Host_Nation_y + OOA,
                        data = jury_data))
# my_model_jury <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_jury <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_jury)

##########################
## Performance Features ##
##########################

# stepwise 
min_model <- lm(Points ~ 1, data = jury_data[,-c(1,2)])
max_model <- formula(lm(Points ~ TC_PerfType_Group  + TC_PerfType_Solo +
                          TC_SingerGender_Female + TC_SingerGender_Male +
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN_n +
                          key_2 + key_3 + key_4 + key_5 + key_6 + key_7 + key_8 + key_11 + mode_1 +
                          time_signature_4 + danceability + energy + loudiness + speechiness +
                          acousticness + instrumentalness + liveness + valence + tempo + duration_ms,
                        data = jury_data))
# my_model_jury <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_jury <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_jury)
anova(my_model_jury)
vif(my_model_jury)

######################
## External Factors ##
######################

# Fit Stepwise Model
min_model <- lm(Points ~ 1, data = jury_data[,-c(1,2)])
max_model <- formula(lm(formula = Points ~ VBlocs1_FC_1 + VBlocs1_FC_18 +
                          VBlocs2_FC_1 + VBlocs2_FC_3 + VBlocs2_FC_4 + 
                          VBlocs2_FC_5 + VBlocs2_FC_6 + VBlocs1_TC_1 + 
                          VBlocs1_TC_3 + VBlocs1_TC_8 + VBlocs1_TC_9 +
                          VBlocs1_TC_13 + VBlocs2_TC_1 + VBlocs2_TC_2 + 
                          VBlocs2_TC_3 + VBlocs2_TC_4 + VBlocs2_TC_5 +
                          VBlocs2_TC_6 + ComVBlocs1_y + ComVBlocs2_y +
                          FC_LANGFAM_Germanic + FC_LANGFAM_ItalianRomance +
                          FC_LANGFAM_Slavic + FC_LANGFAM_Uralic + 
                          TC_LANGFAM_Germanic + TC_LANGFAM_Slavic +
                          ComLANGFAM_y + Neighbours_y + TC_NumNeigh + FC_NonCOB +
                          FC_NonCitzens + METRIC_Citizens + CAP_DIST_km,
                        data = jury_data))
# my_model_jury <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_jury <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_jury)
vif(my_model_jury)

#################
## All Factors ##
#################

# stepwise 
min_model <- lm(Points ~ 1, data = jury_data[,-c(1,2)])
max_model <- formula(lm(Points ~ Average_Points + 
                          key_4 + liveness + key_3 + TC_PerfType_Solo + TC_SONGLANG_English + TC_SONGLANG_Mixed +
                          VBlocs2_TC_4 + VBlocs1_TC_3 + ComLANGFAM_y + ComVBlocs1_y + TC_NumNeigh,
                        data = jury_data))
# my_model_jury <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
my_model_jury <- step(min_model, direction = 'both', scope = max_model, steps = 100)
summary(my_model_jury)
vif(my_model_jury)

#################
## Final Model ##
#################

my_model_jury <- lm(formula = Points ~ VBlocs2_TC_4 + key_3 + TC_PerfType_Solo + 
                      liveness + ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])
summary(my_model_jury)
vif(my_model_jury)

#######################################################################################################################
## Model Evaluation OVERVIEW ############################################################################################
#######################################################################################################################

# NOTE: the models will be evaluated using the car package
# MLR requires the residuals to be ~ IID N(0, sigma^2)
# the residuals will be standardised for the accessment
# Normality Assumptions will be accessed using:
# (i) Normality tests from the nortest package
# (ii) Visualisations such as histograms, QQ-plots, Residual Plots and Add Variable Plots
# Constant Variance will be accessed using:
# (i) non-constand variance test
# Multicollinearity will be accessed using:
# (i) variance inflation factors
# Outliers will be accessed using:
# (i) Cooks Distance

######################################################################################################################
## Overall Model #####################################################################################################
######################################################################################################################

#-- Fit the Final Model --#

my_model_overall <- lm(formula = Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                         ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])
summary(my_model_overall)

#########################################
## Transformation of Response Variable ##
#########################################

# To improve the normality assumptions of the model I shall perform a box-cox transformation on the model
# box-cox transformation using car

bct <- boxCox(object = my_model_overall)

p <- bct$x[which.max(x = bct$y)]
bctPoints <- (((processed_data$Points)^p) - 1)/(p)
my_model_overall <- lm(bctPoints ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                         ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])

summary(my_model_overall)

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardised Residuals --#

sresid <- studres(my_model_overall) 

# Residual vs fits plot
plot(x = my_model_overall$fitted.values, 
     y = sresid, 
     main = "Standardised Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Standardised Residuals")
abline(h = 0, col = "red")

#-- Influencial Obervations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_overall) 
which(sresid < -2)

#qq plot for studentized resid
qqPlot(my_model_overall, 
       main = "QQ Plot")  

# leverage plots
leveragePlots(my_model_overall) 

# Added variable Plots
avPlots(my_model_overall)

# Residual Plots
residualPlots(my_model_overall)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(processed_data)-length(my_model_overall$coefficients)-2)) 
# Crooks Distance plot
plot(my_model_overall, 
     which = 4, 
     cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_overall,	
              id.method = "identify", 
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

#-- Normality Assumption --#

# Normality Test
# Ho: The data is normally distributed
# Ha: the data is not normally distributed
shapiro.test(sresid)
ad.test(sresid)
cvm.test(sresid)
lillie.test(sresid)
pearson.test(sresid)
sf.test(sresid)
# the data is not normally distributed

# Histogram of residuals
hist(sresid, 
     freq = FALSE, 
     main = "Distribution of Standardised Residuals", 
     ylim = c(0,0.4),
     xlim = c(-4, 3))
xfit <- seq(min(sresid), max(sresid), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_overall, 
       ylab = "Standardised Residuals", 
       main = "QQ-Plot of Overall Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_overall)
bptest(my_model_overall)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_overall,
                main = "Spread-Level Plot for Overall Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_overall)  
sqrt(vif(my_model_overall)) > 2
# No signs of collinearity

########################################################################################################################
## TELEVOTE MODEL ######################################################################################################
########################################################################################################################

my_model_tele <- lm(formula = Points ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])

#########################################
## Transformation of Response Variable ##
#########################################

# To improve the normality assumptions of the model I shall perform a box-cox transformation on the model
# box-cox transformation using car

bct <- boxCox(object = my_model_tele)
p <- bct$x[which.max(x = bct$y)]
bctPoints <- (((televote_data$Points)^p) - 1)/(p)
my_model_tele <- lm(bctPoints ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
summary(my_model_tele)

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardised Residuals --#

sresid <- studres(my_model_tele) 

# Residual vs fits plot
plot(x = my_model_tele$fitted.values, 
     y = sresid, 
     main = "Standardised Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Standardised Residuals")
abline(h = 0, col = "red")

#-- Influencial Obervations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_tele) 

#qq plot for studentized resid
qqPlot(my_model_tele, 
       main = "QQ Plot")  

# leverage plots
leveragePlots(my_model_tele) 

# Added variable Plots
avPlots(my_model_tele)

# Residual Plots
residualPlots(my_model_tele)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(processed_data) - length(my_model_tele$coefficients) - 2)) 

plot(my_model_tele, 
     which = 4, 
     cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_tele,	
              id.method = "identify", 
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

#-- Normality Assumption --#

# Normality Test
# Ho: The data is normally distributed
# Ha: the data is not normally distributed
shapiro.test(sresid)
ad.test(sresid)
cvm.test(sresid)
lillie.test(sresid)
pearson.test(sresid)
sf.test(sresid)
# the data is not normally distributed

# Histogram of residuals
hist(sresid, 
     freq = FALSE, 
     main = "Distribution of Studentised Residuals")
xfit <- seq(min(sresid), max(sresid), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_tele, 
       ylab = "Standardised Residuals", 
       main = "QQ-Plot of Televote Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_tele)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_tele,
                main = "Spread-Level Plot for Televote Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_tele)  
sqrt(vif(my_model_tele)) > 2
# No signs of collinearity

########################################################################################################################
## JURY MODEL ##########################################################################################################
########################################################################################################################

my_model_jury <- lm(formula = Points ~ VBlocs2_TC_4 + key_3 + TC_PerfType_Solo + 
                      liveness + ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])

summary(my_model_jury)

#########################################
## Transformation of Response Variable ##
#########################################

# power transformation of 3/4
my_model_jury <- lm(formula = (Points)^(3/4) ~ VBlocs2_TC_4 + key_3 + TC_PerfType_Solo + 
                      liveness + ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])
summary(my_model_jury)
# NOTE: a box cox transformation resulted in normality but also non-constant variance

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardised Residuals --#

sresid <- studres(my_model_jury) 

# Residual vs fits plot
plot(x = my_model_jury$fitted.values, 
     y = sresid, 
     main = "Standardised Residuals vs Fitted Values",
     xlab = "Fitted Values",
     ylab = "Standardised Residuals")
abline(h = 0, col = "red")

#-- Influencial Obervations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_jury) 

#qq plot for studentized resid
qqPlot(my_model_jury, 
       main = "QQ Plot of Studentised Residuals for Jury Vote Model",
       ylab = "Studentised Residuals")  

# leverage plots
leveragePlots(my_model_jury) 

# Added variable Plots
avPlots(my_model_jury)

# Residual Plots
residualPlots(my_model_jury)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(processed_data) - length(my_model_jury$coefficients) - 2)) 

plot(my_model_jury, 
     which = 4, 
     cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_jury,	
              id.method = "identify", 
              main = "Influence Plot", 
              sub = "Circle size is proportial to Cook's Distance" )

#-- Normality Assumption --#

# Normality Test
# Ho: The data is normally distributed
# Ha: the data is not normally distributed
shapiro.test(sresid)
ad.test(sresid)
cvm.test(sresid)
lillie.test(sresid)
pearson.test(sresid)
sf.test(sresid)
# the data is not normally distributed

# Histogram of residuals
hist(sresid, 
     freq = FALSE, 
     main = "Distribution of Studentised Residuals",
     ylim = c(0, 0.4))
xfit <- seq(min(sresid), max(sresid), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_jury, 
       ylab = "Standardised Residuals", 
       main = "QQ-Plot of Jury Vote Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_jury)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_jury,
                main = "Spread-Level Plot for Jury Vote Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_jury)  
sqrt(vif(my_model_jury)) > 2

##########################################################################################################################
## CONCLUSIONS ###########################################################################################################
##########################################################################################################################

#-- Script Overview --#

# car will be used to performa box-cox transformation of the response variable
library(car)

# This script specialises in analysing the models and drawing inferences for the research question
# The research question requires me to answer whether voting blocs, echo nest music features and migration patterns
# can explain the points and voting patterns of the 2016 ESC
# This can be converted into a statistical problem using multiple linear regression whereby I am interested in whether
# voting blocs, echo nest music features and migration patterns significantly explain the points and voting patterns
# This question will be answer by
# construicting t-tests
# Observing the signs of the estimated coefficients
# measuring the increase in variance (R-sq) with the addition of a predictor variable

######################################################################################################################
## Overall Model #####################################################################################################
######################################################################################################################

#-- Fit the Final Model --#

my_model_overall <- lm(formula = Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                         ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])
# perform box-cox transformation
bct <- boxCox(object = my_model_overall)
# return the optimal power transformation
p <- bct$x[which.max(x = bct$y)]
# transform the reponse variable
bctPoints <- (((processed_data$Points)^p) - 1)/(p)
# refit the model with the power transformation
my_model_overall <- lm(bctPoints ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                         ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])
summary(my_model_overall)

#############
## T-tests ##
#############

# Make sure to report 
# (1) the adjusted R-sq value
# (2) the degrees of freedom
# (3) the f statistic for f test
# (4) the p-value for f test
# NOTE: include notation for levels of significance 
# * for 0.05
# ** for 0.01
# *** for 0.001
summary(my_model_overall)

################################
## Directional / Sign effects ##
################################

# Report the sign effects of each significant coeeficient
# + indicates the predictor variable has a positive effect on the dependant variable
# - indicates the predicitor variables  has a negative effect on the dependant variable
summary(my_model_overall)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorportating specific predictor variables
# observe the increase in R-sq when a predicto variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

omodel_ex.vblocs <- lm(formula = bctPoints ~ Average_Points + CAP_DIST_km + 
                         FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                         TC_PerfType_Solo + key_2 + key_6 + time_signature_4 + 
                         key_5 + OOA + speechiness, 
                       data = processed_data[, -c(1, 2)])
summary(omodel_ex.vblocs)

#-- Echo Nest Music Features --#

omodel_ex.music <- lm(bctPoints ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                        FC_NonCitzens + ComLANGFAM_y + METRIC_Citizens + 
                        TC_PerfType_Solo + VBlocs1_TC_13 + 
                        ComVBlocs1_y + VBlocs1_TC_1 + OOA, 
                      data = processed_data[, -c(1, 2)])
# investigate the R-sq value
summary(omodel_ex.music)

#-- Migration Patterns --#

omodel_ex.mig <- lm(formula = Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                      ComLANGFAM_y + liveness + key_3 + 
                      TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                      ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness, 
                    data = processed_data[, -c(1, 2)])
summary(omodel_ex.mig)

########################################################################################################################
## TELEVOTE MODEL ######################################################################################################
########################################################################################################################

my_model_tele <- lm(formula = Points ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
# perform box-cox transformation
bct <- boxCox(object = my_model_tele)
# return the optimal power transformation
p <- bct$x[which.max(x = bct$y)]
# transform the reponse variable
bctPoints <- (((televote_data$Points)^p) - 1)/(p)
# refit the model with the power transformation
my_model_tele <- lm(bctPoints ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
summary(my_model_tele)

#############
## T-tests ##
#############

# Make sure to report 
# (1) the adjusted R-sq value
# (2) the degrees of freedom
# (3) the f statistic for f test
# (4) the p-value for f test
# NOTE: include notation for levels of significance 
# * for 0.05
# ** for 0.01
# *** for 0.001
summary(my_model_tele)

################################
## Directional / Sign effects ##
################################

# Report the sign effects of each significant coeeficient
# + indicates the predictor variable has a positive effect on the dependant variable
# - indicates the predicitor variables  has a negative effect on the dependant variable
summary(my_model_tele)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorportating specific predictor variables
# observe the increase in R-sq when a predicto variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

tmodel_ex.vblocs <- lm(bctPoints ~ METRIC_Citizens + Average_Points + 
                         mode_1 + key_11 + OOA + acousticness + danceability + 
                         key_7 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
# investigate the R-sq value
summary(tmodel_ex.vblocs)

#-- Echo Nest Musical Features --#

tmodel_ex.music <- lm(bctPoints ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                        VBlocs2_TC_1 + OOA + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
# investigate the R-sq value
summary(tmodel_ex.music)

#-- Migration Patterns --#

tmodel_ex.mig <- lm(bctPoints ~  Average_Points + VBlocs1_TC_3 + 
                      VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                      key_7 + VBlocs1_TC_13 + ComLANGFAM_y, data = televote_data[,-c(1, 2)])
# investigate the R-sq value
summary(tmodel_ex.mig)

########################################################################################################################
## JURY MODEL ##########################################################################################################
########################################################################################################################

# power transformation of 3/4
my_model_jury <- lm(formula = (Points)^(3/4) ~ VBlocs2_TC_4 + key_3 + TC_PerfType_Solo + 
                      liveness + ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])
summary(my_model_jury)

#############
## T-tests ##
#############

# Make sure to report 
# (1) the adjusted R-sq value
# (2) the degrees of freedom
# (3) the f statistic for f test
# (4) the p-value for f test
# NOTE: include notation for levels of significance 
# * for 0.05
# ** for 0.01
# *** for 0.001
summary(my_model_jury)

################################
## Directional / Sign effects ##
################################

# Report the sign effects of each significant coeeficient
# + indicates the predictor variable has a positive effect on the dependant variable
# - indicates the predicitor variables  has a negative effect on the dependant variable
summary(my_model_jury)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorportating specific predictor variables
# observe the increase in R-sq when a predicto variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

jmodel_ex.vblocs <- lm(formula = (Points)^(3/4) ~ key_3 + TC_PerfType_Solo + 
                         liveness + ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])
# investigate the R-sq value
summary(jmodel_ex.vblocs)

#-- Echo Nest Musical Features --#

jmodel_ex.music <- lm(formula = (Points)^(3/4) ~ VBlocs2_TC_4 + TC_PerfType_Solo + 
                        ComVBlocs1_y + ComLANGFAM_y, data = jury_data[, -c(1, 2)])
# investigate the R-sq value
summary(jmodel_ex.music)

# investigate the R-sq value
summary(jmodel_ex.mig)
