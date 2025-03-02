source('utilities/extract_preds_by_cats.R')
source('utilities/extract_sign_preds.R')

#' Stepwise Linear Model
#' 
#' @description Fits a linear model using stepwise selection
#' 
#' @param dataset The dataset to fit a linear model to
#' @param pred_cols The predictor columns to use within the linear model
#' @param direction The direction to apply stepwise selection, default is "both"
#' @param steps The number of steps to apply for the stepwise selection process, default is 100
#' @param trace The trace value for determining the amount of logging, default is 0
#' 
#' @return Returns a list with the stepwise selection model results
#' 
step_lm_model <- function(dataset, pred_cols, direction = 'both', steps = 100, trace = 0){
  # create an empty list to hold results
  out_list <- list()
  # fit the model
  # define the minimum model 
  out_list$min_model <- lm(Points ~ 1, data = dataset)
  # create a model formula
  out_list$model_form <- as.formula(paste('Points ~', paste(pred_cols, collapse = ' + ')))
  # create the maximum model
  out_list$max_model <- formula(lm(out_list$model_form, data = dataset))
  # my_model_overall <- step(min_model, direction = 'forward', scope = max_model, steps = 100)
  out_list$my_model_overall <- step(out_list$min_model, direction = direction, scope = out_list$max_model, steps = steps, trace = 0)
  # get model summary
  out_list$summary <- summary(out_list$my_model_overall)
  # get anova results
  out_list$anova <- anova(out_list$my_model_overall)
  # if the model have more than 2 terms
  if (length(out_list$my_model_overall$coefficients) > 2){
    # see variance inflation factors
    out_list$vif <- vif(out_list$my_model_overall)
  }
  # extract out significant predictors
  out_list$sign_factors <- extract_sign_preds(model = out_list$my_model_overall)
  # return the output list
  return(out_list)
}