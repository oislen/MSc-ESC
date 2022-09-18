library(MASS)

plot_residuals_vs_fits <- function(model, main = "Standardised Residuals vs Fitted Values", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # create standardize residuals
  sresid <- studres(model) 
  
  # Residual vs fits plot
  plot(x = model$fitted.values, 
       y = sresid, 
       main = main, 
       xlab = "Fitted Values", 
       ylab = "Standardised Residuals"
       )
  
  # add red horizontal line through y-axis 0
  abline(h = 0, col = "red")
  
  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)
  
}