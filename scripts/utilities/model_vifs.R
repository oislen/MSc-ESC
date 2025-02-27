#' Model Variance Inflation Factors
#' 
#' @description Generates variance inflation factors for a give linear model
#' 
#' @param model The linear model to generate variance inflation factors for
#' @param sign_threshold A threshold for determining significance, default is 2
#' 
#' @return Returns the variance inflation factors as a dataframe
#' 
model_vifs <- function(model, sign_threshold = 2) {
  model_vifs <- vif(model)  
  model_vifs_sign <- sqrt(model_vifs) > sign_threshold
  model_vifs_df <- as.data.frame(cbind(model_vifs, model_vifs_sign))
  colnames(model_vifs_df) <- c("VIF", "sqrt(VIF) > 2")
  return(model_vifs_df)
}