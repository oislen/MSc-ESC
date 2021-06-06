########################################################################################################################
## SECTION 2 - EXPLORATORY ANALYSIS ####################################################################################
########################################################################################################################

# This section is specifically for visualizing and deriving descriptive statistics on the data
# In order to understand the underlying the structures and patterns in the data

# A variety of descriptive statistics will be generated for both the numeric data and the categorical data
# Similarly a variety of visualizations will be generated for both the numeric data and the categorical data
# Further more some visualization on the patterns of diaspora will be generated using Social Networks

###################
## Preliminaries ##
###################

#-- Libraries --#

# Load in relevant libraries
# igraph will be used for SNA
library(igraph)
# ggplot2 will be used for data visualization
library(ggplot2)
library(dplyr)

#-- Data --#

# set the working directory
setwd(file.path(getwd(), 'GitHub/MSc-ESC/MSc_2018'))
# load in the raw ESC 2016 data for the analysis
ESCdata <- read.csv(file = "Data/ESC_2016_voting_data.csv", header = T)
# load in custom utility functions
source("Scripts/utilities/column_to_factor.R")
source("Scripts/utilities/factor_descriptive_statistics.R")
source("Scripts/utilities/numeric_descriptive_statistics.R")
source("Scripts/utilities/plot_bar_chart.R")
source("Scripts/utilities/plot_histogram.R")
source("Scripts/utilities/chisq_assoc_test.R")
source("Scripts/utilities/plot_scatter.R")
source("Scripts/utilities/corr_tests.R")
source("Scripts/utilities/pred_corr_tests.R")
source("Scripts/utilities/graph_network.R")

#-- Some Initial Data Processing --#

# define vectors for all numeric columns
avg_point_num <- c('Average_Points')
mig_num <- c('FC_NonCOB', 'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens', 'METRIC_COBCit')
mus_num <- c('danceability', 'loudiness', 'liveness', 'energy', 'speechiness', 'acousticness', 'instrumentalness', 'acousticness', 'valence', 'tempo', 'duration_ms')
demo_num <- c('TC_NumNeigh', 'FC_CAP_LON', 'FC_CAP_LAT', 'TC_CAP_LON', 'TC_CAP_LAT', 'FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'CAP_DIST_km')
comp_num <- c('OOA')
# consolidate all numeric columns
all_num <- c(avg_point_num, mig_num, mus_num, demo_num, comp_num)

# define vectors for all categorical columns
voting_factors <- c('From_country', 'To_country', 'Points')
comp_factors <- c('Round', 'Voting_Method', 'Host_Nation', 'OOA')
ext_factors <- c('VBlocs1_TC', 'VBlocs2_TC', 'VBlocs1_FC', 'VBlocs2_FC', 'ComVBlocs1', 'ComVBlocs2', 'FC_LANGFAM', 'TC_LANGFAM', 'ComLANGFAM', 'Neighbours', 'TC_NumNeigh')
perf_factors <- c('FC_SONGLANG', 'TC_SONGLANG','ComSONGLAN', 'TC_PerfType', 'TC_SingerGender', 'key', 'mode', 'time_signature')
# consolidate all factor columns
all_factors <- c(voting_factors, comp_factors, ext_factors, perf_factors)

##############################
#-- Descriptive Statistics --#
##############################

# Here I shall derive relevant descriptive statistics for the exploratory analysis
# To do this I shall define two function which save the descriptive statistics in twp separate data frames
# specifically for categorical data and numeric data

#-- CATEGORICAL VARIABLES --#

# NOTE: could code up a table of descriptive statistics for continuous and discrete variables, metadata
# factor_descriptive_statistics(dataset = reduceddata)
full_data_factor_descriptive_statistics_df <- factor_descriptive_statistics(dataset = ESCdata, col_names = all_factors)
# View(full_data_factor_descriptive_statistics_df)
write.csv(x = full_data_factor_descriptive_statistics_df, file = "Report/Stats/categorical_descriptive_statistics.csv")

#-- CONTINUOUS FEATURES --#

# generate numeric descriptive statistics
full_data_numeric_descriptive_statistics_df <- numeric_descriptive_statistics(dataset = ESCdata, col_names = all_num)
# View(full_data_numeric_descriptive_statistics_df)
write.csv(x = full_data_numeric_descriptive_statistics_df, file = "Report/Stats/numeric_descriptive_statistics.csv")

##################################################
#-- Individual Categorical Variable Bar Charts --#
##################################################

# In this section I shall generate a variety of data visualizations
# This allows us to see the underlying structures within each variable
# I shall generate bar charts for categorical variables
# call bar chart plotting function
plot_bar_chart(dataset = ESCdata, col_names = all_factors)

#########################################
#-- Individual Numeric Variable Plots --#
#########################################

# In this section I shall generate a variety of data visualizations
# This allows us to see the underlying structures within each variable
# I shall generate histograms for the numeric variables
# call histogram plotting function
plot_histogram(dataset = ESCdata, col_names = all_num)

########################################
#-- CHI-SQUARED TESTS OF ASSOCIATION --#
########################################

# In this section I shall conduct chi-squared tests of association
# for each categorical predictor variable and the response variable
# Hypothesis:
# Ho: x is independent of y
# Ha: x is associated with y

# There are two parts to it
# (1) Create the data frame to store the chi-squared tests
# (2) Fill the data frame with the relevant information

# perform chi-sq tests of association with Points
chisqtestdf <- chisq_assoc_test(data = ESCdata, col_names = all_factors)

# write the data frame to a csv file
write.csv(x = chisqtestdf, file = "Report/Stats/chi_sq_tests_response.csv", row.names = F)

################################################################
#-- Correlation Scatter Plots and Response Correlation Tests --#
################################################################

# plot scatter plot
plot_scatter(data = ESCdata, col_names = all_num)
# Correlation Test
# Ho: x is not correlated with y
# Ha: x is correlated with y
# execute the correlation tests
cor_test_df <- corr_tests(data = ESCdata, col_names = all_num)
# Write the correlation results to  a .csv file
write.csv(x = cor_test_df, file = "Report/Stats/cor_tests_response.csv", row.names = F)

#############################################
#-- Predictor Correlation Plots and Tests --#
#############################################

# run correlation tests for all predictors
c.t. <- pred_corr_tests(ESCdata[all_num])

# output results as a .csv file
write.csv(x = c.t., file = "Report/Stats/cor_tests_predictors.csv", row.names = F)

# Determine which variables are highly correlated
c.t.[c.t.$Correlation > 0.8 & c.t.$Correlation < 1,]

#######################
#-- Social Networks --#
#######################

# NOTE: that there is more data missing for METIC_Citizens than METRIC_COB
sum(!is.na(ESCdata$METRIC_COB))
sum(!is.na(ESCdata$METRIC_Citizens))
networkdata <- ESCdata %>% filter(!is.na(METRIC_Citizens))

# A variety of Social Networks based on different closeness metrics given a specified Voting_Method
# Here I shall explore the effect of the televote and the Jury Vote
# For example see below a high diaspora graph
# filter out records for high diaspora in the televote
high_diaspora_df <- networkdata %>% filter(Voting_Method == "T", METRIC_COB > 0.1)
# Construct Social Network
G <- graph_from_data_frame(d = high_diaspora_df[, c('From_country', 'To_country')], directed = T)
E(G)$weight <- as.numeric(high_diaspora_df[, 'Points'])
is_weighted(graph = G)
# plot graph of high diaspora
plot(G, 
     main = "Graph of High Diaspora & Points in Televote", 
     layout = layout.fruchterman.reingold, 
     edge.color = "grey", 
     vertex.color = "orange", 
     vertex.label.color = "black", 
     vertex.size = 10, 
     edge.arrow.size = .5, 
     edge.label = E(G)$weight, 
     edge.label.color = "black")

#-- METRIC_COB --#

# generate a summary of metric cob
summary(networkdata$METRIC_COB)
# (1) Voting_Method = Televote
graph_network(dataset = filter(networkdata, Voting_Method == "T" & METRIC_COB > 0.1), weights = 'Points')
# (2) Voting_Method = Jury
graph_network(dataset = filter(networkdata, Voting_Method == "J" & METRIC_COB > 0.1), weights = 'Points')

#-- METRIC_Citizens --#

# generate a summary of metric citizens
summary(networkdata$METRIC_Citizens)
# (1) Voting_Method = Televote
graph_network(dataset = filter(networkdata, Voting_Method == "T" & METRIC_Citizens > 0.1), weights = 'Points')
# (2) Voting_Method = Jury
# Construct Social Network
graph_network(dataset = filter(networkdata, Voting_Method == "J" & METRIC_Citizens > 0.1), weights = 'Points')

#-- METRIC_COBCit --#

# generate a summary of metric cobcit
summary(networkdata$METRIC_COBCit)
# (1) Voting_Method = Televote
graph_network(dataset = filter(networkdata, Voting_Method == "T" & METRIC_COBCit > 0.15), weights = 'Points')
# (2) Voting_Method = Jury
graph_network(dataset = filter(networkdata, Voting_Method == "J" & METRIC_COBCit > 0.15), weights = 'Points')

# Further subdivide the data into semi-finals and final
# (1) Voting_Method = Televote & Round = sf1
graph_network(dataset = filter(networkdata, Voting_Method == "T" & METRIC_COBCit > 0.25 & Round == "sf1"), weights = 'Points')
# (2) Voting_Method = Jury
graph_network(dataset = filter(networkdata, Voting_Method == "J" & METRIC_COBCit > 0.25 & Round == "sf1"), weights = 'Points')

#-- CAP_DIST_km --#

# generate a summary of capital distance in km
summary(networkdata$CAP_DIST_km)
# (1) Voting_Method = Televote
graph_network(dataset = filter(networkdata, Voting_Method == "T" & CAP_DIST_km < 500), weights = 'Points')
# (2) Voting_Method = Jury
graph_network(dataset = filter(networkdata, Voting_Method == "J" & CAP_DIST_km < 500), weights = 'Points')

#-- Average_Points --#

# generate a summary of average point value
summary(networkdata$Average_Points)
# (1) Voting_Method = Televote
graph_network(dataset = filter(networkdata, Voting_Method == "T" & Average_Points > 8), weights = 'Average_Points')
# (2) Voting_Method = Jury
graph_network(dataset = filter(networkdata, Voting_Method == "J" & Average_Points > 8), weights = 'Average_Points')

