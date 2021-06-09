#######################################################################################################################
## SECTION 5 - Model Evaluation OVERVIEW ############################################################################################
#######################################################################################################################

# NOTE: the models will be evaluated using the car package
# MLR requires the residuals to be ~ IID N(0, sigma^2)
# the residuals will be standardized for the assessment
# Normality Assumptions will be accessed using:
# (i) Normality tests from the nortest package
# (ii) Visualizations such as histograms, QQ-plots, Residual Plots and Add Variable Plots
# Constant Variance will be accessed using:
# (i) non-constant variance test
# Multicollinearity will be accessed using:
# (i) variance inflation factors
# Outliers will be accessed using:
# (i) Cooks Distance


#-- Libraries --#

# the car library will be used for evaluating the models and for a box-cox transformation of the response variables
library(car)
# the MASS library will be used to derive the studenisted residuals from the models
library(MASS)
# the nortest library will be used for the Anderson-Darling Normality Test
library(nortest)
# the lmtest library will be used for Breusch-Pagan Test
library(lmtest)
# set the working directory
setwd(file.path(getwd(), 'GitHub/MSc-ESC/MSc_2018'))

#-- Data --#

# load in the historic voting data for deriving the voting blocs
processed_data <- read.csv(file = "Data/Reference_Data/processed_data.csv", header = T)

# Split the Data sets into the Televote and Jury data sets
televote_data <- processed_data[processed_data$Voting_Method_J == 0,]
jury_data <- processed_data[processed_data$Voting_Method_J == 1,]

######################################################################################################################
## Overall Model #####################################################################################################
######################################################################################################################

#-- Fit the Final Model --#

# load final overall model
my_model_overall <- readRDS("Models/overall_final_model.RDS")

# extract out the model coefficients
overall_model_coeff <- names(my_model_overall$coefficients)[-1]

# recreate the model formula
overall_final_model_form<- as.formula(paste('Points ~', paste(overall_model_coeff, collapse = ' + ')))

# generate model summary
summary(my_model_overall)

#########################################
## Transformation of Response Variable ##
#########################################

# To improve the normality assumptions of the model I shall perform a box-cox transformation on the model
# box-cox transformation using car
bct <- boxCox(object = my_model_overall)

# find optimal box-cox power transformation power
p <- bct$x[which.max(x = bct$y)]

# transform points using the optimal power transformation
bctPoints <- (((processed_data$Points)^p) - 1)/(p)

# recreate the model formula
overall_final_model_bct_form<- as.formula(paste('bctPoints ~', paste(model_coeff, collapse = ' + ')))

# refit final model with with box-cox power transformation
my_model_overall <- lm(formula = overall_final_model_bct_form, data = processed_data)

# generate model summary
summary(my_model_overall)

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardized Residuals --#

# create standardize residuals
sresid <- studres(my_model_overall) 

# Residual vs fits plot
plot(x = my_model_overall$fitted.values, y = sresid, main = "Standardised Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Standardised Residuals")
# add red horizontal line through y-axis 0
abline(h = 0, col = "red")

#-- Influential Observations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_overall) 
# final outlier residuals
which(sresid < -2)

# qq-plot for studentized residuals
qqPlot(my_model_overall, main = "QQ Plot")  

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
plot(my_model_overall, which = 4, cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_overall, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance" )

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
hist(sresid, freq = FALSE, main = "Distribution of Standardised Residuals", ylim = c(0,0.4),  xlim = c(-4, 3))
xfit <- seq(min(sresid, na.rm = TRUE), max(sresid, na.rm = TRUE), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_overall, ylab = "Standardised Residuals", main = "QQ-Plot of Overall Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_overall)
bptest(my_model_overall)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_overall, main = "Spread-Level Plot for Overall Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_overall)  
sqrt(vif(my_model_overall)) > 2
# No signs of collinearity

########################################################################################################################
## TELEVOTE MODEL ######################################################################################################
########################################################################################################################

# load final televote model
my_model_tele <- readRDS("Models/televote_final_model.RDS")

# extract out the model coefficients
tele_model_coeff <- names(my_model_tele$coefficients)[-1]

# recreate the model formula
tele_final_model_form<- as.formula(paste('Points ~', paste(tele_model_coeff, collapse = ' + ')))

# generate model summary
summary(my_model_tele)

#########################################
## Transformation of Response Variable ##
#########################################

# To improve the normality assumptions of the model I shall perform a box-cox transformation on the model
# box-cox transformation using car
bct <- boxCox(object = my_model_tele)

# find optimal box-cox power transformation power
p <- bct$x[which.max(x = bct$y)]

# transform points using the optimal power transformation
bctPoints <- (((televote_data$Points)^p) - 1)/(p)

# recreate the model formula
tele_final_model_bct_form <- as.formula(paste('bctPoints ~', paste(tele_model_coeff, collapse = ' + ')))

# refit final model with with box-cox power transformation
my_model_tele <- lm(formula = tele_final_model_bct_form, data = televote_data)

# generate model summary
summary(my_model_tele)

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardized Residuals --#

# create standardize residuals
sresid <- studres(my_model_tele) 

# Residual vs fits plot
plot(x = my_model_tele$fitted.values, y = sresid, main = "Standardised Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Standardised Residuals")
# add red horizontal line through y-axis 0
abline(h = 0, col = "red")

#-- Influential Observations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_tele) 

#qq plot for studentized residuals
qqPlot(my_model_tele, main = "QQ Plot")  

# leverage plots
leveragePlots(my_model_tele) 

# Added variable Plots
avPlots(my_model_tele)

# Residual Plots
residualPlots(my_model_tele)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(processed_data) - length(my_model_tele$coefficients) - 2)) 
# Crooks Distance plot
plot(my_model_tele, which = 4, cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_tele, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance" )

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
hist(sresid, freq = FALSE, main = "Distribution of Studentised Residuals")
xfit <- seq(min(sresid, na.rm = TRUE), max(sresid, na.rm = TRUE), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_tele, ylab = "Standardised Residuals", main = "QQ-Plot of Televote Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_tele)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_tele, main = "Spread-Level Plot for Televote Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_tele)  
sqrt(vif(my_model_tele)) > 2
# No signs of collinearity

########################################################################################################################
## JURY MODEL ##########################################################################################################
########################################################################################################################

# load final televote model
my_model_jury <- readRDS("Models/jury_final_model.RDS")

# extract out the model coefficients
jury_model_coeff <- names(my_model_jury$coefficients)[-1]

# recreate the model formula
jury_final_model_form<- as.formula(paste('Points ~', paste(jury_model_coeff, collapse = ' + ')))

# generate model summary
summary(my_model_jury)

#########################################
## Transformation of Response Variable ##
#########################################

# transform points using the optimal power transformation
ptPoints <- jury_data$Points^(3/4)

# Note: weird bug occuring for row name 177 / row index 88 (possibly due to column with all zero values)
jury_model_coeff <- jury_model_coeff[jury_model_coeff != "TC_PerfType_Mixed"]

# recreate the model formula
jury_final_model_pt_form <- as.formula(paste('ptPoints ~', paste(jury_model_coeff, collapse = ' + ')))

# refit final model with power transformation of 3/4
# NOTE: a box cox transformation resulted in normality but also non-constant variance
my_model_jury <- lm(formula = jury_final_model_pt_form, data = jury_data)

# generate model summary
summary(my_model_jury)

###################################
## Evaluate the Fit of the Model ##
###################################

#-- Extract Standardized Residuals --#

# create standardize residuals
sresid <- studres(my_model_jury) 

# Residual vs fits plot
plot(x = my_model_jury$fitted.values, y = sresid,  main = "Standardised Residuals vs Fitted Values", xlab = "Fitted Values", ylab = "Standardised Residuals")
# add red horizontal line through y-axis 0
abline(h = 0, col = "red")

#-- Influential Observations --#

# Assessing Outliers
# Bonferonni p-value for most extreme obs
outlierTest(my_model_jury) 

#qq plot for studentized residuals
qqPlot(my_model_jury, main = "QQ Plot of Studentised Residuals for Jury Vote Model", ylab = "Studentised Residuals")  

# leverage plots
leveragePlots(my_model_jury) 

# Added variable Plots
avPlots(my_model_jury)

# Residual Plots
residualPlots(my_model_jury)

# Cook's D plot
# identify D values > 4/(n-k-1) 
cutoff <- 4/((nrow(processed_data) - length(my_model_jury$coefficients) - 2)) 
# Crooks Distance plot 
plot(my_model_jury, which = 4, cook.levels = cutoff)

# Influence Plot 
influencePlot(my_model_jury, id.method = "identify", main = "Influence Plot", sub = "Circle size is proportial to Cook's Distance" )

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
hist(sresid, freq = FALSE, main = "Distribution of Studentised Residuals", ylim = c(0, 0.4))
xfit <- seq(min(sresid, na.rm = TRUE), max(sresid, na.rm = TRUE), length = 40) 
yfit <- dnorm(xfit) 
lines(xfit, yfit)

# QQ-plot of the data
qqPlot(my_model_jury, ylab = "Standardised Residuals", main = "QQ-Plot of Jury Vote Model Standardised Residuals")

#-- Non-Constant Residual Variance --#

# Non-Constant Error Variance Test
# Ho: constant error variance
# Ha: Non-constant error Variance
ncvTest(my_model_jury)

# plot studentized residuals vs. fitted values 
spreadLevelPlot(my_model_jury, main = "Spread-Level Plot for Jury Vote Model")

#-- Multi-Collinearity --# 

# Variance Inflation Factors 
vif(my_model_jury)  
sqrt(vif(my_model_jury)) > 2
