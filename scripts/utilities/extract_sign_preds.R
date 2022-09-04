extract_sign_preds <- function(model, p = 0.05){
  # create model summary
  mod_summary <- summary(model)
  # Pull out coefficients and p-values
  mod_summary_sign <- mod_summary$coefficients[-1 , 'Pr(>|t|)'] 
  # filter coefficients given p value
  sign_preds <- names(which(mod_summary_sign < p))
  # return the results
  return(sign_preds)
}