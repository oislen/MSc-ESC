plot_cooks <- function(model, data, output_fpath = NA){
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  # identify D values > 4/(n-k-1) 
  cutoff <- 4/((nrow(data)-length(model$coefficients)-2))
  # plot cooks distance
  plot(model, which = 4, cook.levels = cutoff)
  if (!is.na(output_fpath)){
    dev.off()
  }  
  return(0)
}