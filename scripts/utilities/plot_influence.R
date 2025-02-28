library(car)

#' Plot Influence
#' 
#' @description Plots residual influence for a given linear model
#' 
#' @param model The model to plot the influence of studentised residuals for
#' @param main The main title of the influence plot, default is "Influence Plot"
#' @param output_fpath The output file path to write the influence plot as a .png file, default is NA
#' 
#' @return Returns 0 for successful execution
#' 
plot_influence <- function(model, main = "Influence Plot", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # Influence Plot 
  car::influencePlot(
    model,
    id.method = "identify", 
    main = main, 
    sub = "Circle size is proportial to Cook's Distance"
    )
  
  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)
  
}