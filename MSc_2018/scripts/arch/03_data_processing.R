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

library(ggplot2)

#-- Data --#

# set the working directory
setwd('C:/Users/User/Documents/GitHub/MSc-ESC')

# load in the raw ESC 2016 data for the analysis
ESCdata <- read.csv(file = "Data/ESC_2016_voting_data.csv", header = T)

# order the data by To_country, From_country, Round and OOA
ESCdata <- ESCdata[order(ESCdata$To_country, 
                         ESCdata$From_country, 
                         ESCdata$Round, 
                         ESCdata$OOA),]

########################
#-- drop ID variable --#
########################

ESCdata <- subset(x = ESCdata, select = -ID)

#################################
#-- Redefine Factor Variables --#
#################################

# Some of the numeric music features need to be redefined as nominal variables
# the variables are key, mode and time signature
to_factor_cols = c('key', 'mode', 'time_signature', 'VBlocs1_FC', 'VBlocs2_FC', 'VBlocs1_TC', 'VBlocs2_TC')
for (col in to_factor_cols){
  ESCdata[, col] <- as.factor(ESCdata[, col])
}

###################################
#-- REMOVE MISSING OBSERVATIONS --#
###################################

# NA values per column
na_count_per_column <- sapply(ESCdata, function(y) sum(length(which(is.na(y)))) / nrow(ESCdata) )

# write to a csv file
write.csv(na_count_per_column, 'Report/Stats/NA_prop_per_columns.csv')

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
write.csv(x = TC_FC_missing_obseration_df,
         file = "Report/Stats/TC_FC_missing_obseration_df.csv"
         )

# The Effect on To_country, From_country, Points
check_cols = c('To_country', 'From_country', 'Points')
check_data = ESCdata[, check_cols]

for (i in 1:length(check_cols)){
  
  # extract the column name
  col_name = check_cols[i]
  
  # create the plot
  plt1 = ggplot(data = ESCdata, mapping = aes(x = ESCdata[, i], fill = ESCdata[, i])) + 
    geom_bar(stat = "count", width = 0.7) + 
    labs(title = paste("All Cases - Bar Chart of ", col_name), x = col_name, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(fill = FALSE)
  
  # save the plot
  ggsave(paste('Report/Plots/Missings/', col_name, 'All_Cases_Bar_Chart.png'))
  
  # print the plot
  print(plt2)
  
  # create the plot
  plt2 = ggplot(data = completedata, mapping = aes(x = completedata[, i], fill = completedata[, i])) + 
    geom_bar(stat = "count", width = 0.7) + 
    labs(title = paste("Complete Cases - Bar Chart of ", col_name), x = col_name, y = "Count") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
    guides(fill = FALSE)
  
  # save the plot
  ggsave(paste('Report/Plots/Missings/', col_name, 'Complete_Cases_Bar_Chart.png'))
  
  # print the plot
  print(plt2)
  
}

#########################################
#-- DIVIDE THE DATA INTO FACTOR BLOCS --#
#########################################

#-- voting_factors --#

voting_factors <- completedata[, c('From_country', 'To_country', 'Points')]
# There 3 variables in the voting factors bloc
ncol(voting_factors)

#-- competition_factors --#

comp_factors_names = c('Average_Points', 'Round', 'Voting_Method', 'Host_Nation', 'OOA')
competition_factors <- completedata[, comp_factors_names]
# There are 5 variables in the competition factors bloc
ncol(competition_factors)

#-- external_factors --#

ext_factors_names = c('VBlocs1_FC', 'VBlocs2_FC', 'VBlocs1_TC', 'VBlocs2_TC', 'ComVBlocs1', 'ComVBlocs2', 
                      'FC_LANGFAM', 'TC_LANGFAM', 'ComLANGFAM', 'Neighbours', 'TC_NumNeigh', 'FC_NonCOB',
                      'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens',
                      'METRIC_COBCit', 'FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'FC_CAP_LAT', 'FC_CAP_LON', 
                      'TC_CAP_LAT', 'TC_CAP_LON', 'CAP_DIST_km')

external_factors <- completedata[, ext_factors_names]
# There are 27 variables in the external factors bloc
ncol(external_factors)

#-- performance_factors --#

perf_factors_names = c('TC_PerfType', 'TC_SingerGender', 'FC_SONGLANG', 'TC_SONGLANG', 'ComSONGLAN', 
                       'danceability', 'energy', 'key', 'loudiness', 'mode', 'speechiness', 'acousticness',
                       'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')
performance_factors <- completedata[,perf_factors_names]
# There are 18 variables in the performance factors bloc
ncol(performance_factors)

##############################################
#-- EXTRACT NUMERIC & CATEGORICAL FEATURES --#
##############################################

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
# (ii) Categorical competition Variables
competition_factors_categorical <- extract_factor_data(dataset = competition_factors)

#-- External Factors --#

# (i) Numeric Variables
external_factors_numeric <- extract_numeric_data(dataset = external_factors)
# (ii) Categorical Variables
external_factors_categorical <- extract_factor_data(dataset = external_factors)

#-- Performance Factors --#

# (i) Numeric Variables
performance_factors_numeric <- extract_numeric_data(dataset = performance_factors)
# (ii) Categorical Variables
performance_factors_categorical <- extract_factor_data(dataset = performance_factors)

#-- Voting Factors --#

# (i) Numeric Variables
voting_factors_numeric <- extract_numeric_data(dataset = voting_factors)
#  (ii) Categorical Variables
voting_factors_categorical <- extract_factor_data(dataset = voting_factors)

################################################
#-- DUMMY ENCODING FOR CATEGORICAL VARIABLES --#
################################################

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

# Competition Factors
competition_factors_categorical <- categorical_dummy_encoding(dataset = competition_factors_categorical)

# External Factors
external_factors_categorical <- categorical_dummy_encoding(dataset = external_factors_categorical)

# Performance Factors
performance_factors_categorical <- categorical_dummy_encoding(dataset = performance_factors_categorical)

################################
#-- STANDARDISE NUMERIC DATA --#
################################

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
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

# (2) External Factors
# external_factors_numeric <- range_standardise_data(dataset = external_factors_numeric, lower_bound = 0, upper_bound = 1)
external_factors_numeric <- normalise_data(dataset = external_factors_numeric)
# Check that each column has mean 0
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

# (3) Performance Factors
# performance_factors_numeric <- range_standardise_data(dataset = performance_factors_numeric, lower_bound = 0, upper_bound = 1)
performance_factors_numeric <- normalise_data(dataset = performance_factors_numeric)
# Check that each column has mean 0
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

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

#######################
#-- Data Reducation --#
#######################

# I shall perform a data reduction in this section
# Here I shall remove categorcal variables which:
# (1) Have only a single type of observation, 0 or 1
# (2) Variables that form part of a linear combination
# (3) Categorical Varibles that are strongly Associated
# (4) Numeric Variables that are trongly Correlated

#-- Remove Categorical Variables with no Observations --#

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

#-- Variables of Linear Combinations --#

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


#-- Remove Unncessary Categorical Variables via Chi-squared Tests --#

# If two variables are feature a lot of 0s or 1s 
# then there will be a strong association between the two variables
# as they share a lot of common observations
# thus one of the variables can be removed as they both measure the same entity
# this lowers the chance of collinearity and reduces the number of dimensions

# First lets screen how many observations exist in each variable
apply(X = external_factors_categorical, 
      MARGIN = 2,
      FUN = sum)

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
  chisqtestdf <- as.data.frame(matrix(ncol = 5))
  # rename the columns of the data frame
  colnames(chisqtestdf) <- c("X", "Y", "X-Obs", "Y-Obs", "P-Value")
  #-- PART 2
  # r represents the row index and will be used to input the relevent data
  r = 1
  for (i in 1:data_frame_rows) {
    j = i + 1
    while (j <= data_frame_rows) {
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
      j = j + 1
    }
  }
  # Set the output of the function to be the chi-squared test dataframe
  return(chisqtestdf)
}

#-- Voting Blocs --#

# (1) FC_VBlocs1
FC_VBlocs1_facts = c('VBlocs1_FC_1', 'VBlocs1_FC_2', 'VBlocs1_FC_5', 'VBlocs1_FC_7', 'VBlocs1_FC_8', 'VBlocs1_FC_10',
                     'VBlocs1_FC_15', 'VBlocs1_FC_17', 'VBlocs1_FC_21')
# Subset the data to be tested
FC_VBlocs_1df <- external_factors_categorical[,FC_VBlocs1_facts]
# Run the Chi-Squared Tests
chisq_tests_FC_VBlocs1 <- chisq_tests(dataset = FC_VBlocs_1df)
# Filter out the significant p-values
chisq_tests_FC_VBlocs1[chisq_tests_FC_VBlocs1$'P-Value' < 0.05 & chisq_tests_FC_VBlocs1$`X-Obs` < 66,]

# (2) FC_VBlocs2
FC_VBlocs2_facts = c('VBlocs2_FC_1', 'VBlocs2_FC_2', 'VBlocs2_FC_3', 'VBlocs2_FC_4', 'VBlocs2_FC_5', 'VBlocs2_FC_6')
# Subset the data to be tested
FC_VBlocs_2df <- external_factors_categorical[,FC_VBlocs2_facts]
# Run the Chi-Squared Tests
chisq_tests_FC_VBlocs2 <- chisq_tests(dataset = FC_VBlocs_2df)
# Filter out the significant p-values
chisq_tests_FC_VBlocs2[chisq_tests_FC_VBlocs2$'P-Value' < 0.05 & chisq_tests_FC_VBlocs2$`X-Obs` < 66,]

# (3) TC_VBlocs1
TC_VBlocs1_facts = c('VBlocs1_TC_1', 'VBlocs1_TC_2', 'VBlocs1_TC_3', 'VBlocs1_TC_4', 'VBlocs1_TC_5',
                     'VBlocs1_TC_6', 'VBlocs1_TC_7', 'VBlocs1_TC_8', 'VBlocs1_TC_9', 'VBlocs1_TC_10',
                     'VBlocs1_TC_11', 'VBlocs1_TC_13', 'VBlocs1_TC_15',
                     'VBlocs1_TC_16', 'VBlocs1_TC_17', 'VBlocs1_TC_18', 'VBlocs1_TC_19',
                     'VBlocs1_TC_21')
# Subset the data to be tested
TC_VBlocs_1df <- external_factors_categorical[,TC_VBlocs1_facts]
# Run the Chi-Squared Tests
chisq_tests_TC_VBlocs1 <- chisq_tests(dataset = TC_VBlocs_1df)
# Filter out the significant p-values
chisq_tests_TC_VBlocs1[chisq_tests_TC_VBlocs1$'P-Value' < 0.05 & chisq_tests_TC_VBlocs1$`X-Obs` < 66,]

# (4) TC_VBlocs2
TC_VBlocs2_facts = c('VBlocs2_TC_1', 'VBlocs2_TC_2', 'VBlocs2_TC_3', 'VBlocs2_TC_4', 'VBlocs2_TC_5', 'VBlocs2_TC_6')
# Subset the data to be tested
TC_VBlocs_2df <- external_factors_categorical[,TC_VBlocs2_facts]
# Run the Chi-Squared Tests
chisq_tests_TC_VBlocs2 <- chisq_tests(dataset = TC_VBlocs_2df)
# Filter out the significant p-values
chisq_tests_TC_VBlocs2[chisq_tests_TC_VBlocs2$'P-Value' < 0.05 & chisq_tests_TC_VBlocs2$`X-Obs` < 66,]

# Remove Unneccessary Categorical Variables 
external_factors_categorical <- subset(external_factors_categorical,
                                       select = -c(VBlocs1_FC_2, VBlocs1_FC_5, VBlocs1_FC_7, VBlocs1_FC_8,
                                                   VBlocs1_FC_10, VBlocs1_FC_15, VBlocs1_FC_17, VBlocs1_FC_21,
                                                   VBlocs2_FC_2, VBlocs1_TC_2, VBlocs1_TC_4, VBlocs1_TC_5,
                                                   VBlocs1_TC_6, VBlocs1_TC_7, VBlocs1_TC_10, VBlocs1_TC_11,
                                                   VBlocs1_TC_15, VBlocs1_TC_16, VBlocs1_TC_17, VBlocs1_TC_18, 
                                                   VBlocs1_TC_19, VBlocs1_TC_21))

#-- Language Family --#

# Subset the data to be tested
# (1)FC_LANGFAM
FC_LANGFAM_facts = c('FC_LANGFAM_Baltic', 'FC_LANGFAM_Germanic', 'FC_LANGFAM_ItalicRomance', 'FC_LANGFAM_Slavic', 'FC_LANGFAM_Uralic')
FC_LANGFAM_df <- external_factors_categorical[,FC_LANGFAM_facts]
# (2) TC_LANGFAM
TC_LANGFAM_facts = c('TC_LANGFAM_Armenian', 'TC_LANGFAM_Baltic', 'TC_LANGFAM_Germanic',
                     'TC_LANGFAM_Hellenic', 'TC_LANGFAM_ItalicRomance', 'TC_LANGFAM_Semetic',
                     'TC_LANGFAM_Semitic', 'TC_LANGFAM_Slavic', 'TC_LANGFAM_Turkic', 'TC_LANGFAM_Uralic')
TC_LANGFAM_df <- external_factors_categorical[, TC_LANGFAM_facts]
# Run the Chi-Squared Tests
chisq_tests_FC_LANGFAM_df <- chisq_tests(dataset = FC_LANGFAM_df)
chisq_tests_TC_LANGFAM_df <- chisq_tests(dataset = TC_LANGFAM_df)
# Filter out the significant p-values
chisq_tests_FC_LANGFAM_df[chisq_tests_FC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_FC_LANGFAM_df$`X-Obs` < 66,]
chisq_tests_TC_LANGFAM_df[chisq_tests_TC_LANGFAM_df$'P-Value' < 0.05 & chisq_tests_TC_LANGFAM_df$`X-Obs` < 66,]

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
FC_SONGLANG_facts = c('FC_SONGLANG_English', 'FC_SONGLANG_French', 'FC_SONGLANG_Mixed')
FC_SONGLANG_df <- performance_factors_categorical[, FC_SONGLANG_facts]
# (2) TC_SONGLANG
TC_SONGLANG_facts = c('TC_SONGLANG_Bosnian', 'TC_SONGLANG_English', 'TC_SONGLANG_French', 'TC_SONGLANG_Macedonian', 'TC_SONGLANG_Mixed')
TC_SONGLANG_df <- performance_factors_categorical[,TC_SONGLANG_facts]
# Run the Chi-Squared Tests
chisq_tests_FC_SONGLANG_df <- chisq_tests(dataset = FC_SONGLANG_df)
chisq_tests_TC_SONGLANG_df <- chisq_tests(dataset = TC_SONGLANG_df)
# Filter out the significant p-values
chisq_tests_FC_SONGLANG_df[chisq_tests_FC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_FC_SONGLANG_df$`X-Obs` < 66,]
chisq_tests_TC_SONGLANG_df[chisq_tests_TC_SONGLANG_df$'P-Value' < 0.05 & chisq_tests_TC_SONGLANG_df$`X-Obs` < 66,]

# Remove Unneccessary Categorical Variables 
performance_factors_categorical <- subset(x = performance_factors_categorical,
                                          select = -c(FC_SONGLANG_French, FC_SONGLANG_Mixed,
                                                      TC_SONGLANG_Bosnian, TC_SONGLANG_French,
                                                      TC_SONGLANG_Macedonian))

#-- Keys --#

# Subset the data to be tested
keys_fact = c('key_0', 'key_1', 'key_2', 'key_3', 'key_4', 'key_5', 'key_6', 'key_7', 'key_8', 'key_9', 'key_10', 'key_11')
keys_df <- performance_factors_categorical[, keys_fact]
# Run the Chi-Squared Tests
chisq_tests_keys_df <- chisq_tests(dataset = keys_df)
# Filter out the significant p-values
chisq_tests_keys_df[chisq_tests_keys_df$'P-Value' < 0.05 & chisq_tests_keys_df$`X-Obs` < 66,]

# Remove Unneccessary Categorical Variables 
performance_factors_categorical <- subset(x = performance_factors_categorical,
                                          select = -c(key_0, key_1, key_9, key_10))

#####################################################################
#-- Remove Unncessary Categorical Variables via Correlation Tests --#
#####################################################################

# If two numeric variables are very highly correlated
# and represent the same entity
# then it is unncessary to include them in the data modelling stage
# This is particular the case for the migration data

# NOTE: I shall impelement the same correlation test function
# that was used during the exploratory analysis section

#-- Function Definition --#

correlation_tests <- function (dataset) {
  # Create a data frame to hold the correlation test data
  cor_test_df <- as.data.frame(matrix(ncol = 4))
  # Name the columns of the Correlation Test Data Frame
  colnames(cor_test_df) <- c("X", "Y", "Correlation", "P-Value")
  # Create a row index to populate the data frame with
  r = 1
  for (i in 1:ncol(dataset)) {
    j = i + 1
    while (j  <= ncol(dataset)) {
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
      j = j + 1
    }
  }
  # return the cor_test_df
  return(cor_test_df)
}

#-- Migration Data --#

# Perform the correlation tests
mig_nums = c('FC_NonCOB', 'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB','METRIC_Citizens', 'METRIC_COBCit')
mig_df = external_factors_numeric[, mig_nums]
c.t. <- correlation_tests(dataset = mig_df)
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

###############################
#-- FINAL PROCESSED DATASET --#
###############################

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

# write the processed data to a csv file
write.csv(processed_data,
          'Data/Reference_Data/processed_data.csv',
          row.names = F
          )
