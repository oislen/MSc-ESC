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