#######################################################################################################################
## SECTION 4 - Data Modelling OVERVIEW ################################################################################
#######################################################################################################################

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
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN +
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
                          key_7 + ComSONGLAN + FC_SONGLANG_English + key_5 + key_2 + 
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
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN +
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
                          FC_SONGLANG_English + TC_SONGLANG_English + TC_SONGLANG_Mixed + ComSONGLAN +
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
