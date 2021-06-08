# load custom functions
source('Scripts/utilities/extract_preds_by_cats.R')
source('Scripts/utilities/extract_sign_preds.R')
# define a function to fit a step-wise linear model
step_lm_model <- function(dataset, pred_cols, direction = 'both', steps = 100){
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
  out_list$my_model_overall <- step(out_list$min_model, direction = direction, scope = out_list$max_model, steps = steps)
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