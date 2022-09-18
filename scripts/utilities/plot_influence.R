library(car)

plot_influence <- function(model, main = "Influence Plot", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # Influence Plot 
  influencePlot(model, id.method = "identify", 
                main = main, 
                sub = "Circle size is proportial to Cook's Distance"
                )
  
  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)
  
}