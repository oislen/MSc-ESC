##########################################################################################################################
## SECTION 6 - CONCLUSIONS ###############################################################################################
##########################################################################################################################

#-- Script Overview --#

# This script specialises in analysing the models and drawing inferences for the research question
# The research question requires me to answer whether voting blocs, echo nest music features and migration patterns
# can explain the points and voting patterns of the 2016 ESC
# This can be converted into a statistical problem using multiple linear regression whereby I am interested in whether
# voting blocs, echo nest music features and migration patterns significantly explain the points and voting patterns
# This question will be answer by
# construicting t-tests
# Observing the signs of the estimated coefficients
# measuring the increase in variance (R-sq) with the addition of a predictor variable


#-- Libraries --#

# the car library will be used for evaluting the models and for a box-cox transformation of the response variables
library(car)

#-- Data --#

# set the working directory
setwd(dir = "C:/Users/User/Documents/GitHub/MSc-ESC")

# load in the historic voting data for deriving the voting blocs
processed_data <- read.csv(file = "Data/Reference_Data/processed_data.csv", header = T)

# Split the Datasets into the Televote and Jury datasets
televote_data <- processed_data[processed_data$Voting_Method_J == 0,]
jury_data <- processed_data[processed_data$Voting_Method_J == 1,]


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

