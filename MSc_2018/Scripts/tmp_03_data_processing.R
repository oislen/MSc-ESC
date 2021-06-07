#######################################################################################################################
## SECTION 3 - DATA PROCESSING ########################################################################################
#######################################################################################################################

# In this script I shall process the ESC data for the data zmodeling stage
# This will incorporate the following:
# (1) Redefine variables as factor or numeric
# (2) Dividing the variables into the three predefined groups; 
# (i) Performance
# (ii) External 
# (iii) Competition
# (3) Normalize the Numeric Data to have mean 0 and standard deviation 1
# (4) Dummy Encoding all Categorical Factor levels.
# (5) Data Reduction
# (i) Redundant Variables
# (ii) Variables of Linear Combinations
# (iii) Categorical Variables via Chi-Squared Tests of Association

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
# (ii) data modeling


#-- Libraries --#

# load required libraries
library(ggplot2)
library(dplyr)
# set the working directory
setwd(file.path(getwd(), 'GitHub/MSc-ESC/MSc_2018'))
# load in custom utility functions
source("Scripts/utilities/column_to_factor.R")
source("Scripts/utilities/TC_FC_missing_observations.R")
source("Scripts/utilities/plot_missings_effect.R")
source("Scripts/utilities/extract_numeric_data.R")
source("Scripts/utilities/extract_factor_data.R")
source("Scripts/utilities/categorical_dummy_encoding.R")
source("Scripts/utilities/range_standardise_data.R")
source("Scripts/utilities/normalise_data.R")
source("Scripts/utilities/range_normalisation.R")
source("Scripts/utilities/data_normalisation.R")
source("Scripts/utilities/chisq_tests.R")

#-- Data --#

# load in the raw ESC 2016 data for the analysis
ESCdata <- read.csv(file = "Data/ESC_2016_voting_data.csv", header = T)

# order the data by To_country, From_country, Round and OOA
ESCdata <- ESCdata %>% arrange(To_country, From_country, Round, OOA)

########################
#-- drop ID variable --#
########################

ESCdata <- ESCdata %>% select(-c(id))

#################################
#-- Redefine Factor Variables --#
#################################

# Some of the numeric music features need to be redefined as nominal variables
# the variables are key, mode and time signature
# define the columns to be converted to factor variables
to_factor_cols <- c('key', 'mode', 'time_signature', 'VBlocs1_FC', 'VBlocs2_FC', 'VBlocs1_TC', 'VBlocs2_TC')
# call the column to factor function 
ESCdata <- column_to_factor(dataset = ESCdata, col_names = to_factor_cols)

###################################
#-- REMOVE MISSING OBSERVATIONS --#
###################################

# NA values per column
na_count_per_column <- sapply(ESCdata, function(y) sum(length(which(is.na(y)))) / nrow(ESCdata))
# write to a csv file
write.csv(na_count_per_column, 'Report/Stats/NA_prop_per_columns.csv')
# there are 1022 rows with missing data values
nrow(ESCdata) - nrow(ESCdata %>% na.omit)
# This accounts for just over 60% of the data
(nrow(ESCdata) - nrow(ESCdata %>% na.omit)) / nrow(ESCdata) * 100
# filter our rows with missing data values
completedata <- ESCdata %>% na.omit
# have 658 complete observations
nrow(completedata)

# extract out unique from countries in order
from_countries <- unique(ESCdata$From_countr) %>% sort()
# generate count stats for the from countries before and after missings are removed
TC_FC_missing_obseration_df <- TC_FC_missing_observations(from_countries = from_countries, orig_data = ESCdata, comp_data = completedata)
# There is a substantial amount of data being lost here
# This is by far a major limitation in the research
# definitely a possible area of improvement for future research
# We could use an alternative source such as the world bank
# http://www.worldbank.org/en/topic/migrationremittancesdiasporaissues/brief/migration-remittances-data
# however these do not store the data based on country of birth / citizenship
# write the TC_FC_missing_observations_df to a csv file
write.csv(x = TC_FC_missing_obseration_df, file = "Report/Stats/TC_FC_missing_obseration_df.csv")

# The Effect on To_country, From_country, Points
voting_factors <- c('To_country', 'From_country', 'Points')
# call the plot missing function
plot_missings_effect(col_name = voting_factors, orig_data = ESCdata, comp_data = completedata)

#########################################
#-- DIVIDE THE DATA INTO FACTOR BLOCS --#
#########################################

#-- voting_factors --#

# extract all the voting factors
voting_factors <- completedata %>% subset(select = voting_factors)
# There 3 variables in the voting factors bloc
ncol(voting_factors)

#-- competition_factors --#

# define the competition factor column names
comp_factors_names = c('Average_Points', 'Round', 'Voting_Method', 'Host_Nation', 'OOA')
# extract the competition factor column names
competition_factors <- completedata %>% subset(select = comp_factors_names)
# There are 5 variables in the competition factors bloc
ncol(competition_factors)

#-- external_factors --#

# define the external factor column names
ext_factors_names = c('VBlocs1_FC', 'VBlocs2_FC', 'VBlocs1_TC', 'VBlocs2_TC', 'ComVBlocs1', 'ComVBlocs2', 
                      'FC_LANGFAM', 'TC_LANGFAM', 'ComLANGFAM', 'Neighbours', 'TC_NumNeigh', 'FC_NonCOB',
                      'FC_NonCitzens', 'FC_COB', 'FC_Citizens', 'FC_Population', 'METRIC_COB', 'METRIC_Citizens',
                      'METRIC_COBCit', 'FC_GDP_mil', 'TC_GDP_mil', 'GDP_PROP', 'FC_CAP_LAT', 'FC_CAP_LON', 
                      'TC_CAP_LAT', 'TC_CAP_LON', 'CAP_DIST_km')
# extract the external factor column names
external_factors <- completedata %>% subset(select = ext_factors_names)
# There are 27 variables in the external factors bloc
ncol(external_factors)

#-- performance_factors --#

# define the performance factor column names
perf_factors_names = c('TC_PerfType', 'TC_SingerGender', 'FC_SONGLANG', 'TC_SONGLANG', 'ComSONGLAN', 
                       'danceability', 'energy', 'key', 'loudiness', 'mode', 'speechiness', 'acousticness',
                       'instrumentalness', 'liveness', 'valence', 'tempo', 'duration_ms', 'time_signature')
# extract the performance factor column names
performance_factors <- completedata %>% subset(select = perf_factors_names)
# There are 18 variables in the performance factors bloc
ncol(performance_factors)

##############################################
#-- EXTRACT NUMERIC & CATEGORICAL FEATURES --#
##############################################

# want to extract the numeric & categorical features for each variable bloc
# I shall define two functions that extract the numeric variables and
# the categorical variables separately

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

# NOTE: It is not necessary to dummy encode the voting factors To_country and From_country
# as we will not be incorporating the into our analysis, as they are not appropriate

# Competition Factors
competition_factors_categorical <- categorical_dummy_encoding(dataset = competition_factors_categorical)

# External Factors
external_factors_categorical <- categorical_dummy_encoding(dataset = external_factors_categorical)

# Performance Factors
performance_factors_categorical <- categorical_dummy_encoding(dataset = performance_factors_categorical)

################################
#-- STANDARDISE NUMERIC DATA --#
################################

# In this section I shall normalize or range standardized 
# the numeric variables in each block
# There is also a special case for OOA in the competition Bloc
# OOA will need to be standardized in relation to each round
# I shall do this after I have standardized the numeric variables from each bloc

#-- Standardize the Data --#

# (1) Competition Factors
# competition_factors_numeric <- range_standardize_data(dataset = competition_factors_numeric, lower_bound = 0, upper_bound = 1)
competition_factors_numeric <- normalise_data(dataset = competition_factors_numeric)
# Check that each column has mean 0
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = competition_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

# (2) External Factors
# external_factors_numeric <- range_standardize_data(dataset = external_factors_numeric, lower_bound = 0, upper_bound = 1)
external_factors_numeric <- normalise_data(dataset = external_factors_numeric)
# Check that each column has mean 0
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = external_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

# (3) Performance Factors
# performance_factors_numeric <- range_standardize_data(dataset = performance_factors_numeric, lower_bound = 0, upper_bound = 1)
performance_factors_numeric <- normalise_data(dataset = performance_factors_numeric)
# Check that each column has mean 0
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = mean), digits = 6)
# Check that each column has standard deviation 1
round(apply(X = performance_factors_numeric, MARGIN = 2, FUN = sd), digits = 6)

#-- OOA --#

# Extract the relevant features
OOA_df <- completedata %>% subset(select = c('From_country', 'To_country', 'Round', 'OOA'))
# Order the Features
OOA_df <- OOA_df %>% arrange(Round, OOA, To_country, From_country)
# check head of results
head(OOA_df)

# Apply the range standardization
OOA_df[OOA_df$Round == "f", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "f", 4], lb = 0, ub = 1)
OOA_df[OOA_df$Round == "sf1", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "sf1", 4], lb = 0, ub = 1)
OOA_df[OOA_df$Round == "sf2", 4] <- range_normalisation(dataset = OOA_df[OOA_df$Round == "sf2", 4], lb = 0, ub = 1)
head(OOA_df)

# Re-Order the data frame
OOA_df <- OOA_df %>% arrange(To_country, From_country, Round, OOA)
# check the results
head(OOA_df)
# compare with original
head(completedata %>% subset(select = c('From_country', 'To_country', 'Round', 'OOA')))
# check target to overwrite
head(competition_factors_numeric)
# Overwrite the OOA in competition_factors_numeric with the new standardized OOA
competition_factors_numeric[,2] <- OOA_df[,4]

######################
#-- Data Reduction --#
######################

# I shall perform a data reduction in this section
# Here I shall remove categorical variables which:
# (1) Have only a single type of observation, 0 or 1
# (2) Variables that form part of a linear combination
# (3) Categorical Variables that are strongly Associated
# (4) Numeric Variables that are strongly Correlated

#-- Remove Categorical Variables with no Observations --#

# Here I shall remove categorical variables which have only 0 observations

#-- External Factors --#

# There are 22 categorical variables that have only 0 observations
zero_ext_fact_cats <- apply(X = external_factors_categorical, MARGIN = 2, FUN = sum) == 0
zero_ext_fact_cats_cols <- names(which(zero_ext_fact_cats))
non_zero_ext_fact_cols <- colnames(external_factors_categorical)[!(colnames(external_factors_categorical) %in% zero_ext_fact_cats_cols)]
length(zero_ext_fact_cats_cols)
# Caution below statement
# The following external categorical variables have only 0 observations
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
# Also dropping sparse language family attributes
# extract the external categorical features
external_factors_categorical <- external_factors_categorical %>% subset(select = non_zero_ext_fact_cols)

#-- Competition Factors --#

# There are no competition categorical variables with only 0 observations
sum(apply(X = competition_factors_categorical, MARGIN = 2, FUN = sum) == 0)

#-- Performance Factors--#

# There are 2 performance categorical variables with only 0 observations
sum(apply(X = performance_factors_categorical,  MARGIN = 2, FUN = sum) == 0)

# The following are the performance categorical variables with only 0 observations
# (1) FC_SONGLANG_Bosnian
# (2) FC_SONGLANG_Macedonian
sum(apply(X = performance_factors_categorical,  MARGIN = 2,FUN = sum) == 0)

#-- Variables of Linear Combinations --#

# Remove all categorical variables that are the binary opposites
# (1) Performance variables
performance_factors_categorical <- performance_factors_categorical %>% subset(select = -c(mode_0, time_signature_3))
                                  
# (2) competition variables
competition_factors_categorical <- competition_factors_categorical %>% subset(select = -c(Host_Nation_n))

# (3) external variables
external_factors_categorical <- external_factors_categorical %>% subset(select = -c(ComVBlocs1_n, ComVBlocs2_n, ComLANGFAM_n, Neighbours_n))

# NOTE: also remove variables which are a linear combination of other variables
# (1) Competition Variables
competition_factors_categorical <- competition_factors_categorical %>% subset(select = -c(Round_sf2, Voting_Method_T))

# (2) External Variables
performance_factors_categorical <- performance_factors_categorical %>% subset(select = -c(TC_PerfType_Duet))

#-- Remove Unnecessary Categorical Variables via Chi-squared Tests --#

# If two variables are feature a lot of 0s or 1s 
# then there will be a strong association between the two variables
# as they share a lot of common observations
# thus one of the variables can be removed as they both measure the same entity
# this lowers the chance of collinearity and reduces the number of dimensions

# First lets screen how many observations exist in each variable
apply(X = external_factors_categorical, MARGIN = 2, FUN = sum)

#-- Voting Blocs --#

# (1) FC_VBlocs1
FC_VBlocs1_facts = c('VBlocs1_FC_1', 'VBlocs1_FC_2', 'VBlocs1_FC_5', 'VBlocs1_FC_7', 'VBlocs1_FC_8', 'VBlocs1_FC_10', 'VBlocs1_FC_15', 'VBlocs1_FC_17', 'VBlocs1_FC_21')
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
