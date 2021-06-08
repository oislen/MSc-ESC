##########################################################################################################################
## SECTION 6 - CONCLUSIONS ###############################################################################################
##########################################################################################################################

#-- Script Overview --#

# This script specializes in analyzing the models and drawing inferences for the research question
# The research question requires me to answer whether voting blocs, echo nest music features and migration patterns
# can explain the points and voting patterns of the 2016 ESC
# This can be converted into a statistical problem using multiple linear regression whereby I am interested in whether
# voting blocs, echo nest music features and migration patterns significantly explain the points and voting patterns
# This question will be answer by
# constricting tests
# Observing the signs of the estimated coefficients
# measuring the increase in variance (R-sq) with the addition of a predictor variable


#-- Libraries --#

# the car library will be used for evaluating the models and for a box-cox transformation of the response variables
library(car)

# set the working directory
setwd(file.path(getwd(), 'GitHub/MSc-ESC/MSc_2018'))

#-- Data --#

# load in the historic voting data for deriving the voting blocs
processed_data <- read.csv(file = "Data/arch/processed_data.csv", header = T)

# Split the Data sets into the Televote and Jury data sets
televote_data <- processed_data[processed_data$Voting_Method_J == 0,]
jury_data <- processed_data[processed_data$Voting_Method_J == 1,]


######################################################################################################################
## Overall Model #####################################################################################################
######################################################################################################################

#-- Fit the Final Model --#

# define the model formula for the final model
my_model_overall_form <- Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                                  FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                                  TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                                  ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness
# fit the final overall model
my_model_overall <- lm(formula = my_model_overall_form, data = processed_data)
# perform box-cox transformation
bct <- boxCox(object = my_model_overall)
# return the optimal power transformation
p <- bct$x[which.max(x = bct$y)]
# transform the response variable
bctPoints <- (((processed_data$Points)^p) - 1)/(p)
# redefine the model formula for the final model
my_model_overall_bct_from <- bctPoints ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                                          FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                                          TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                                          ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness
# refit the model with the power transformation
my_model_overall <- lm(my_model_overall_bct_from, data = processed_data)
# generate final model summary
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

# Report the sign effects of each significant coefficient
# + indicates the predictor variable has a positive effect on the dependent variable
# - indicates the predictor variables  has a negative effect on the dependent variable
summary(my_model_overall)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorporating specific predictor variables
# observe the increase in R-sq when a predictor variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

# define the model formula
omodel_ex.vblocs_form <- bctPoints ~ Average_Points + CAP_DIST_km + 
                                     FC_NonCitzens + ComLANGFAM_y + liveness + key_3 + METRIC_Citizens + 
                                     TC_PerfType_Solo + key_2 + key_6 + time_signature_4 + 
                                    key_5 + OOA + speechiness
# fit the linear model
omodel_ex.vblocs <- lm(formula = omodel_ex.vblocs_form, data = processed_data)
# generate model summary
summary(omodel_ex.vblocs)

#-- Echo Nest Music Features --#

# define the model formula
omodel_ex.music_form <- bctPoints ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                                    FC_NonCitzens + ComLANGFAM_y + METRIC_Citizens + 
                                    TC_PerfType_Solo + VBlocs1_TC_13 + 
                                     ComVBlocs1_y + VBlocs1_TC_1 + OOA
# fit the linear model
omodel_ex.music <- lm(omodel_ex.music_form, data = processed_data)
# investigate the R-sq value
summary(omodel_ex.music)

#-- Migration Patterns --#

# define the model formula
omodel_ex.mig_form <- Points ~ Average_Points + VBlocs1_TC_3 + CAP_DIST_km + 
                               ComLANGFAM_y + liveness + key_3 + 
                               TC_PerfType_Solo + key_2 + VBlocs1_TC_13 + key_6 + time_signature_4 + 
                               ComVBlocs1_y + VBlocs1_TC_1 + key_5 + OOA + speechiness
# fit the linear model
omodel_ex.mig <- lm(formula = omodel_ex.mig_form, data = processed_data)
# generate the model summary
summary(omodel_ex.mig)

########################################################################################################################
## TELEVOTE MODEL ######################################################################################################
########################################################################################################################

# define the model formula
my_model_tele_form <- Points ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                               VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                               key_7 + VBlocs1_TC_13 + ComLANGFAM_y
# fit the linear model
my_model_tele <- lm(formula = my_model_tele_form, data = televote_data)
# perform box-cox transformation
bct <- boxCox(object = my_model_tele)
# return the optimal power transformation
p <- bct$x[which.max(x = bct$y)]
# transform the response variable
bctPoints <- (((televote_data$Points)^p) - 1)/(p)
# define the model formula 
my_model_tele_bct <- bctPoints ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                                 VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                                 key_7 + VBlocs1_TC_13 + ComLANGFAM_y
# refit the model with the power transformation
my_model_tele <- lm(my_model_tele_bct, data = televote_data)
# generate model summary
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

# Report the sign effects of each significant coefficient
# + indicates the predictor variable has a positive effect on the dependent variable
# - indicates the predictor variables  has a negative effect on the defendant variable
summary(my_model_tele)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorporating specific predictor variables
# observe the increase in R-sq when a predictor variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

# define model formula
tmodel_ex.vblocs_form <- bctPoints ~ METRIC_Citizens + Average_Points + 
                                     mode_1 + key_11 + OOA + acousticness + danceability + 
                                     key_7 + ComLANGFAM_y
# fit linear model
tmodel_ex.vblocs <- lm(tmodel_ex.vblocs_form, data = televote_data)
# investigate the R-sq value
summary(tmodel_ex.vblocs)

#-- Echo Nest Musical Features --#

# define model formula
tmodel_ex.music_form <- bctPoints ~ METRIC_Citizens + Average_Points + VBlocs1_TC_3 + 
                                    VBlocs2_TC_1 + OOA + VBlocs1_TC_13 + ComLANGFAM_y
# fit linear model
tmodel_ex.music <- lm(tmodel_ex.music_form, data = televote_data)
# investigate the R-sq value
summary(tmodel_ex.music)

#-- Migration Patterns --#

# define model formula
tmodel_ex.mig_form <- bctPoints ~  Average_Points + VBlocs1_TC_3 + 
                                   VBlocs2_TC_1 + mode_1 + key_11 + OOA + acousticness + danceability + 
                                   key_7 + VBlocs1_TC_13 + ComLANGFAM_y
# fit linear model
tmodel_ex.mig <- lm(tmodel_ex.mig_form, data = televote_data)
# investigate the R-sq value
summary(tmodel_ex.mig)

########################################################################################################################
## JURY MODEL ##########################################################################################################
########################################################################################################################

# power transformation of 3/4
# define model formula
# Note: thesis shows output for non-power transformed case (i.e. Points ~ key_3 + ... )
my_model_jury_form <- (Points)^(3/4) ~ VBlocs2_TC_4 + key_3 + TC_PerfType_Solo + 
                                       liveness + ComVBlocs1_y + ComLANGFAM_y
# fit linear model
my_model_jury <- lm(formula = my_model_jury_form, data = jury_data)
# generate model summary
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

# Report the sign effects of each significant coefficient
# + indicates the predictor variable has a positive effect on the dependent variable
# - indicates the predictor variables  has a negative effect on the dependent variable
summary(my_model_jury)

########################
## Explained Variance ##
########################

# Investigate the increase of variance explained by incorporating specific predictor variables
# observe the increase in R-sq when a predictor variable is included / excluded from the model
# do this for voting blocs, Echo Nest music factors and Migration patterns

#-- Voting Blocs --#

# define model formula
jmodel_ex.vblocs_form <- (Points)^(3/4) ~ key_3 + TC_PerfType_Solo + 
                                          liveness + ComVBlocs1_y + ComLANGFAM_y
# fit linear model
jmodel_ex.vblocs <- lm(formula = jmodel_ex.vblocs_form, data = jury_data)
# investigate the R-sq value
summary(jmodel_ex.vblocs)

#-- Echo Nest Musical Features --#

# define model formula
jmodel_ex.music_form <- (Points)^(3/4) ~ VBlocs2_TC_4 + TC_PerfType_Solo + 
                                         ComVBlocs1_y + ComLANGFAM_y
# fit linear model
jmodel_ex.music <- lm(formula = jmodel_ex.music_form, data = jury_data)
# investigate the R-sq value
summary(jmodel_ex.music)

