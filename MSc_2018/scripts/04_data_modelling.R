#######################################################################################################################
## SECTION 4 - Data Modelling OVERVIEW ################################################################################
#######################################################################################################################

# A stratified analysis method will be implemented
# (1) Test Overall Data
# Create baseline model based on research
# Create my own model, iteratively
# Compare
# (2) Split Data by Voting Method
# Due to a lack of data I can't stratify the data by each country
# Need a minimum of 10/20 observations per covariate for regression analysis
# Split data by voting method, research televote shows more bias than the jury
# NOTE forward and step wise fitting will be utilized using AIC to determine model of best fit

# NOTE: This script is a direct continuation on from the ESC_Data_Processing_II script

# NOTE: the models will be evaluated using the car package
# MLR requires the residuals to be ~ IID N(0, sigma^2)
# the residuals will be standardized for the assessment
# Normality Assumptions will be accessed using:
# (i) Normality tests from the nortest package
# (ii) Visualizations such as histograms, QQ-plots, Residual Plots and Add Variable Plots
# Constant Variance will be accessed using:
# (i) non-constant variance test
# Multi-collinearity will be accessed using:
# (i) variance inflation factors
# Outliers will be accessed using:
# (i) Cooks Distance


#-- Libraries --#

# the car library will be used for evaluating the models and for a box-cox transformation of the response variables
library(car)
library(dplyr)
# set the working directory
setwd(file.path(getwd(), 'GitHub/MSc-ESC/MSc_2018'))
# load custom functions
source('Scripts/utilities/step_lm_model.R')

#-- Data --#

# load in the historic voting data for deriving the voting blocs
processed_data <- read.csv(file = "Data/Reference_Data/processed_data.csv", header = T)

# Split the Data sets into the Televote and Jury data sets
televote_data <- processed_data[processed_data$Voting_Method_J == 0,]
jury_data <- processed_data[processed_data$Voting_Method_J == 1,]

# define the competition factors
competition_factors <- extract_preds_by_cats(cat = 'competition')
# define the performance factors
performance_factors <- extract_preds_by_cats(cat = 'performance')
# define the external factors
external_factors <- extract_preds_by_cats(cat = 'external')

######################################################################################################################
## My Model - Overall Data ###########################################################################################
######################################################################################################################

# iteratively create my own mode; 
# continuously noting the magnitude and significance of the predictors. 
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

# fit step wise linear model
overall_competition_model <- step_lm_model(dataset = processed_data, pred_cols = competition_factors)
# extract out significant predictors
overall_sign_competition_factors <- overall_competition_model$sign_factors

#########################
## Performance Factors ##
#########################

# fit step wise linear model
overall_performance_model <- step_lm_model(dataset = processed_data, pred_cols = performance_factors)
# extract out significant predictors
overall_sign_performance_factors <- overall_performance_model$sign_factors

######################
## External Factors ##
######################

# fit step wise linear model
overall_external_model <- step_lm_model(dataset = processed_data, pred_cols = external_factors)
# extract out significant predictors
overall_sign_external_factors <- overall_external_model$sign_factors

#################
## All Factors ##
#################

# define the significant factors
overall_all_factors <- c(overall_sign_competition_factors, overall_sign_performance_factors, overall_sign_external_factors)
# fit step wise linear model
overall_all_model <- step_lm_model(dataset = processed_data, pred_cols = overall_all_factors)
# extract out significant predictors
overall_sign_all_factors <- overall_all_model$sign_factors

#################
## Final Model ##
#################

# create a model formula
overall_final_model_form <- as.formula(paste('Points ~', paste(overall_sign_all_factors, collapse = ' + ')))
# create the maximum model
overall_final_model <- lm(overall_final_model_form, data = processed_data)
# get model summary
summary(overall_final_model)
# get anova results
anova(overall_final_model)
# check variance inflation factors
vif(overall_final_model)
# write the model to disk
saveRDS(object = overall_final_model, file = 'Models/overall_final_model.RDS')

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

# fit step wise linear model
televote_competition_model <- step_lm_model(dataset = televote_data, pred_cols = competition_factors)
# extract out significant predictors
televote_sign_competition_factors <- televote_competition_model$sign_factors

##########################
## Performance Features ##
##########################

# fit step wise linear model
televote_performance_model <- step_lm_model(dataset = televote_data, pred_cols = performance_factors)
# extract out significant predictors
televote_sign_performance_factors <- televote_performance_model$sign_factors

######################
## External Factors ##
######################

# fit step wise linear model
televote_external_model <- step_lm_model(dataset = televote_data, pred_cols = external_factors)
# extract out significant predictors
televote_sign_external_factors <- televote_external_model$sign_factors

#################
## All Factors ##
#################

# define the significant factors
televote_all_factors <- c(televote_sign_competition_factors, televote_sign_performance_factors, televote_sign_external_factors)
# fit step wise linear model
televote_all_model <- step_lm_model(dataset = televote_data, pred_cols = televote_all_factors)
# extract out significant predictors
televote_sign_all_factors <- televote_all_model$sign_factors

#################
## Final Model ##
#################

# create a model formula
televote_final_model_form <- as.formula(paste('Points ~', paste(televote_sign_all_factors, collapse = ' + ')))
# create the maximum model
televote_final_model <- lm(televote_final_model_form, data = televote_data)
# get model summary
summary(televote_final_model)
# get anova results
anova(televote_final_model)
# check variance inflation factors
vif(televote_final_model)
# write the model to disk
saveRDS(object = televote_final_model, file = 'Models/televote_final_model.RDS')

#######################################################################################################################
## My Model - Split by Jury ###########################################################################################
#######################################################################################################################

#########################
## Competition Factors ##
#########################

# fit step wise linear model
jury_competition_model <- step_lm_model(dataset = jury_data, pred_cols = competition_factors)
# extract out significant predictors
jury_sign_competition_factors <- jury_competition_model$sign_factors

##########################
## Performance Features ##
##########################

# fit step wise linear model
jury_performance_model <- step_lm_model(dataset = jury_data, pred_cols = performance_factors)
# extract out significant predictors
jury_sign_performance_factors <- jury_performance_model$sign_factors

######################
## External Factors ##
######################

# fit step wise linear model
jury_external_model <- step_lm_model(dataset = jury_data, pred_cols = external_factors)
# extract out significant predictors
jury_sign_external_factors <- jury_external_model$sign_factors

#################
## All Factors ##
#################

# define the significant factors
jury_all_factors <- c(jury_sign_competition_factors, jury_sign_performance_factors, jury_sign_external_factors)
# fit step wise linear model
jury_all_model <- step_lm_model(dataset = jury_data, pred_cols = jury_all_factors)
# extract out significant predictors
jury_sign_all_factors <- jury_all_model$sign_factors

#################
## Final Model ##
#################

# create a model formula
jury_final_model_form <- as.formula(paste('Points ~', paste(jury_sign_all_factors, collapse = ' + ')))
# create the maximum model
jury_final_model <- lm(jury_final_model_form, data = jury_data)
# get model summary
summary(jury_final_model)
# get anova results
anova(jury_final_model)
# check variance inflation factors
vif(jury_final_model)
# write the model to disk
saveRDS(object = jury_final_model, file = 'Models/jury_final_model.RDS')
