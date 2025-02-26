#' Extract Significant Predictors
#' 
#' @description Extracts the significant predictor variables from a linear model
#' 
#' @param model The linear model to extract significant predictor variables from
#' @param p The significance level from which to extract predictor variables with
#' 
#' @return Returns the significant predictor variables as a vector
#' 
extract_sign_preds <- function(model, p = 0.05){
  # create model summary
  mod_summary <- summary(model)
  # Pull out coefficients and p-values
  mod_summary_sign <- mod_summary$coefficients[-1 , "Pr(>|t|)"] 
  # filter coefficients given p value
  sign_preds <- names(which(mod_summary_sign < p))
  # return the results
  return(sign_preds)
}