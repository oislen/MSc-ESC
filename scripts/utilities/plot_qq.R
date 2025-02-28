#' Plot QQ
#' 
#' @description Plots a QQ chart for a given linear model
#' 
#' @param model The model to plot the qq plot for
#' @param main The main title of the qq plot, default is "QQ-Plot of Overall Model Standardised Residuals"
#' @param output_fpath The output file path to write the qq plot as a .png file, default is NA
#' 
#' @return Returns 0 for successful execution
#' 
plot_qq <- function(model, main = "QQ-Plot of Overall Model Standardised Residuals", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # QQ-plot of the data
  qqPlot(model, ylab = "Standardised Residuals", main = main)
  
  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)

}