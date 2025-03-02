#' Plot Dendrogram Chart
#' 
#' @description Plots a dendrogram chart for a network community
#' 
#' @param community The network community to plot the dendrogram plot for
#' @param main The main title of the dendrogram plot
#' @param output_fpath The output file path to write the dendrogram plot as a .png file, default is NA
#' 
#' @return Returns 0 for successful execution
#' 
plot_dendrogram_chart <- function(
    community,
    main,
    output_fpath=NA
    ){
  # if saving file as jpeg
  if (!is.na(output_fpath)){
    jpeg(output_fpath, width = 3000, height = 3000, quality = 350, pointsize = 60)
  }
  # plot dendrogram
  plot_dendrogram(x = community, main = main, mode = "hclust", ann = TRUE)
  if (!is.na(output_fpath)){
    dev.off()
  }
  return(0)
}