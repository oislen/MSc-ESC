library(MASS)

boxcox_transplot <- function(model, output_fpath = NA){
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