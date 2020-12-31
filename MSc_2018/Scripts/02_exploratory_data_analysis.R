########################################################################################################################
## SECTION 2 - EXPLORATORY ANALYSIS ####################################################################################
########################################################################################################################

# This section is specifically for visualising and deriving descriptive statistics on the data
# In order to understand the underlying the structures and patterns in the data

# A variety of descriptive statistics will be generated for both the numeric data and the categorical data
# Similarly a variety of visualisations will be generated for both the numeric data and the categorical data
# Further more some visualisation on the patterns of diaspora will be generated using Social Networks


###################
## Prelimineries ##
###################

#-- Libraries --#

# Load in relevant libraries
# igraph will be used for SNA
library(igraph)
# ggplot2 will be used for data visuaisation
library(ggplot2)

#-- Data --#

# set the working directory
setwd('C:/Users/User/Documents/GitHub/MSc-ESC')

# load in the raw ESC 2016 data for the analysis
ESCdata <- read.csv(file = "Data/ESC_2016_voting_data.csv", header = T)

#-- Some Intial Data Processing --#

# Some of the numeric music features need to be redefined as nominal variables
# the variables are key, mode and time signature
to_factor_cols = c('key', 'mode', 'time_signature', 'VBlocs1_FC', 'VBlocs2_FC', 'VBlocs1_TC', 'VBlocs2_TC')
for (col in to_factor_cols){
  ESCdata[, col] <- as.factor(ESCdata[, col])
}

##############################
#-- Descriptive Statistics --#
##############################

# Here I shall derive relevant descriptive statistics for the exploratory analysis
# To do this I shall define two function which save the descriptive statistics in twp seperate data frames
# specifically for categorical data and numeric data

#-- CATEGORICAL VARIABLES --#

# define a function to calculate descriptive statistics for categorical varibales
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
          file = "../Report/Stats/categorical_descriptive_statistics.csv")

#-- CONTINUOUS FEATURES --#

# define a function to calculate descriptive statistics for continuous varibales
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
          file = "../Report/Stats/numeric_descriptive_statistics.csv")

##################################################
#-- Individual Categorical Variable Bar Charts --#
##################################################

# In this section I shall generate a variety of data visualisations
# This allows us to see the underlying structures within each variable
# I shall generate bar charts for categorical variables


voting_factors = c('From_country', 'To_country', 'Points')
comp_factors = c('Round', 'Voting_Method', 'Host_Nation', 'OOA')
ext_factors = c('VBlocs1_TC', 'VBlocs2_TC', 'VBlocs1_FC', 'VBlocs2_FC', 'ComVBlocs', 'ComVBlocs2', 'LANGFAM', 'ComLANGFAM', 'Neighbours', 'TC_NumNeigh')
perf_factors = c('ComSONGLAN', 'key', 'mode', 'time_signature', 'TC_PerfType', 'TC_SingerGender')
all_factors = c(voting_factors, comp_factors, comp_factors, perf_factors)
all_factors_data = ESCdata[all_factors]

# use a for loop to generate a bar plot for each categorical attributes
for (idx in 1:length(all_factors)){
  
  # extract the column
  col_chr <- all_factors[idx]
  
  # create the ploy
  plt = ggplot(data = all_factors_data, 
               mapping = aes(x = as.factor(all_factors_data[,idx]), 
                             fill = as.factor(all_factors_data[,idx])
                             )
               ) + 
          geom_bar(stat = "count", width = 0.7) + 
          labs(title = paste("Bar Chart of ", col_chr), x = col_chr, y = "Count") +
          theme_minimal() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
          guides(fill = FALSE)
  
  # print the plot
  print(plt)
  
  # save plot
  ggsave(paste('Report/Plots/Bar_Charts/', col_chr,'_bar_chart.png'))
  
}

#########################################
#-- Individual Numeric Variable Plots --#
#########################################

# In this section I shall generate a variety of data visualisations
# This allows us to see the underlying structures within each variable
# I shall generate histograms for the numeric variables

avg_point_num = c('Average_Points')
mig_num = c('FC_NonCOB', 'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens', 'METRIC_COBCit')
mus_num = c('danceability', 'loudiness', 'energy', 'speechiness', 'acousticness', 'instrumentalness', 'acousticness', 'valence', 'tempo', 'duration_ms')
demo_num = c('FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'CAP_DIST_km')
all_num = c(avg_point_num, mig_num, mus_num, demo_num)
all_numeric_data = ESCdata[all_num]

# use a for loop to generate histograms for each numeric attribute
for (idx in 1:length(all_num)){
  
  # extract the column
  col_chr <- all_num[idx]
  
  # Histogram of Average_Points
  plt = ggplot(data = all_numeric_data, 
               mapping = aes(x = all_numeric_data[,idx])) + 
          geom_histogram(col = "black", fill = "steelblue") + 
          labs(title = paste("Histogram of ", col_chr), x = col_chr, y = "Total") + 
          theme_minimal()
  
  # print the plot
  print(plt)
  
  # save plot
  ggsave(paste('Report/Plots/Histograms/', col_chr,'_histogram.png'))
  
}

########################################
#-- CHI-SQUARED TESTS OF ASSOCIATION --#
########################################

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
          file = "Report/Stats/chi_sq_tests_response.csv",
          row.names = F)

#################################
#-- Correlation Scatter Plots --#
#################################

avg_point_num = c('Average_Points')
mig_num = c('FC_NonCOB', 'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens', 'METRIC_COBCit')
mus_num = c('danceability', 'loudiness', 'energy', 'speechiness', 'acousticness', 'instrumentalness', 'acousticness', 'valence', 'tempo', 'duration_ms')
demo_num = c('TC_NumNeigh', 'FC_CAP_LON', 'FC_CAP_LAT', 'TC_CAP_LON', 'TC_CAP_LON', 'TC_CAP_LAT', 'FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'CAP_DIST_km')
comp_num = c('OOA')
all_cor_num = c('Points', avg_point_num, mig_num, mus_num, demo_num, comp_num)
all_cor_numeric_data = ESCdata[all_cor_num]

# use a for loop to generate correlation scatter plots
for (idx in 1:length(all_cor_num)){
  
  # extract the column
  col_chr <- all_cor_num[idx]
  
  # create the xaxis limits
  lims = c("1","2","3","4","5","6","7","8","","10","","12")
  
  # create the corelation scatter plot
  plt = ggplot(data = all_cor_numeric_data, 
               mapping = aes(x = Points, 
                             y = all_cor_numeric_data[,idx]
                             )
               ) + 
          geom_point(shape = 16, colour = "blue") + 
          labs(title = paste("Scatterplot of Points vs ",col_chr), 
               x = "Points", 
               y = col_chr
               )  +  
          geom_smooth(method ='lm', 
                      linetype = "dashed", 
                      color="darkred", 
                      fill = "red"
                      ) +
          scale_x_discrete(limits = lims) + 
          theme_minimal()
  
  # print the plot
  print(plt)
  
  # save plot
  ggsave(paste('Report/Plots/Scatterplots/', col_chr,'_vs_Points_Scatterplot_.png'))
  
}

##################################
#-- Response Correlation Tests --#
##################################

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
          file = "Report/Stats/cor_tests_response.csv",
          row.names = F)

#############################################
#-- Predictor Correlation Plots and Tests --#
#############################################

# IMPORTANT: Correlation does not imply causality

# Define a function to perform Correlation Tests
correlation_tests <- function (dataset) {
  # extract the unique column names
  n_cols = ncol(dataset)
  dataset_cols = unique(colnames(dataset))
  cor_test_df <- as.data.frame(matrix(ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:n_cols) {
    j = i + 1
    while (j <= n_cols) {
      # Perform Correlation Test
      c.t. <- cor.test(x = dataset[,dataset_cols[i]],
                       y = dataset[,dataset_cols[j]],
                       na.action = "na.omit")
      # Fill in the X Variable Name
      cor_test_df[r, 1] <- dataset_cols[i]
      # Fill in the Y Variable Name
      cor_test_df[r, 2] <- dataset_cols[j]
      # Fill in the correlation
      #cor_test_df[r, 3] <- round(c.t.$estimate, digits = 5)
      cor_test_df[r, 3] <- round(cor(x = dataset[,dataset_cols[i]],
                                     y = dataset[,dataset_cols[j]],
                                     use = "complete.obs"), 
                                 digits = 5)
      # Fill in the p-value
      cor_test_df[r, 4] <- round(c.t.$p.value, digits = 5)
      # Update the row index
      r = r + 1
      # Update the j index
      j = j + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}

comp_num = c('Average_Points', 'OOA')
perf_num = c('danceability', 'energy', 'loudiness', 'speechiness', 'acousticness', 'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms')
ext_num = c('TC_NumNeigh', 'FC_NonCOB', 'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens', 'METRIC_COBCit', 'FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'FC_CAP_LAT', 'FC_CAP_LON', 'TC_CAP_LAT', 'TC_CAP_LON', 'CAP_DIST_km')
all_cor_test_num = c(comp_num, perf_num, ext_num)

# Correlation Tests
c.t. <- correlation_tests(ESCdata[all_cor_test_num])

write.csv(x = c.t.,
          file = "Report/Stats/cor_tests_predictors.csv",
          row.names = F)

# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,]

######################
#-- Socai Networks --#
######################

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
