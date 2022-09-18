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