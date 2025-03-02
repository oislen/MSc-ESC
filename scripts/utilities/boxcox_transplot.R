library(MASS)

#' Box-Cox Transformation and Plot
#' 
#' @description Performs a Box-Cox transformation and generates a plot of the transformation results against a linear model
#' 
#' @param model A linear model to perform a Box-Cox transformation on
#' @param output_fpath A string indicating the file path to save the Box-Cox transformation plot to disk, default is NA
#' 
#' @return Returns a Box-Cox transformation object
#' 
boxcox_transplot <- function(
    model,
    output_fpath=NA
    ){
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  bct <- MASS::boxcox(object = model, plotit = TRUE)
  if (!is.na(output_fpath)){
    dev.off()
  }  
  return(bct)
}