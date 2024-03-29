#######################################################################################################################
## SECTION 5 - Model Evaluation OVERVIEW ############################################################################################
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


#-- Libraries --#

# the car library will be used for evaluting the models and for a box-cox transformation of the response variables
library(car)
# the MASS library will be used to derive the studenisted residuals from the models
library(MASS)
# the nortest library will be used for the Anderson-Darling Normality Test
library(nortest)

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
