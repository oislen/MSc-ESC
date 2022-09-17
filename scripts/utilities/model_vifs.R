model_vifs <- function(model) {
  model_vifs <- vif(model)  
  model_vifs_sign <- sqrt(model_vifs) > 2
  model_vifs_df <- as.data.frame(cbind(model_vifs, model_vifs_sign))
  colnames(model_vifs_df) <- c("VIF", "sqrt(VIF) > 2")
  return(model_vifs_df)
}