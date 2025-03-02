#' Plot Spread Level
#' 
#' @description Plots a spread level plot for a given linear model
#' 
#' @param model The linear model to plot the spread level plot for
#' @param main The main title of the spread level plot, default is "Spread-Level Plot for Overall Model"
#' @param output_fpath The output file path to write the spread level plot as a .png file, default is NA
#' 
#' @return Returns 0 for successful execution
#' 
plot_spreadlevel <- function(model, main = "Spread-Level Plot for Overall Model", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # QQ-plot of the data
  spreadLevelPlot(model, main = main)
  
  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)
  
}