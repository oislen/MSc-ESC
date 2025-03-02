library(MASS)

#' Plot Residual vs Fits
#' 
#' @description Plots scatter plot of model residuals vs fitted values
#' 
#' @param model The linear model to plot residuals vs fitted values
#' @param main The main title of the scatter plot, default is "Standardised Residuals vs Fitted Values"
#' @param output_fpath The output file path to write the scatter plot as a .png file, default is NA
#' 
#' @return Returns 0 for successful execution
#' 
plot_residuals_vs_fits <- function(model, main = "Standardised Residuals vs Fitted Values", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # create standardize residuals
  sresid <- MASS::studres(model) 
  
  # Residual vs fits plot
  plot(
    x = model$fitted.values, 
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