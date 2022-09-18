plot_residualdist <- function(sresid, main = "Distribution of Standardised Residuals", output_fpath = NA){
  
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  
  # Histogram of residuals
  hist(sresid, freq = FALSE, main = main, ylim = c(0,0.4),  xlim = c(-4, 3))
  xfit <- seq(min(sresid, na.rm = TRUE), max(sresid, na.rm = TRUE), length = 40) 
  yfit <- dnorm(xfit) 
  lines(xfit, yfit)

  if (!is.na(output_fpath)){
    dev.off()
  }  
  
  return(0)
  
}