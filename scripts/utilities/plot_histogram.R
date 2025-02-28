library(ggplot2)

#' Plot Histogram
#' 
#' @description Plots a histogram for a given set of columns in a dataset
#' 
#' @param dataset The dataframe with numeric features
#' @param col_names The numeric features to plot with a histogram
#' @param output_dir The output directory to write the histogram as a .png file, default is 'report/plots/histograms'
#' 
#' @return Returns 0 for successful execution
#' 
plot_histogram <- function(dataset, col_names, output_dir = 'report/plots/histograms'){
  # use a for loop to generate histograms for each numeric attribute
  for (col in col_names){
    
    # Histogram of Average_Points
    plt = ggplot(data = dataset, 
                 mapping = aes(x = dataset[, col])) + 
      geom_histogram(fill = "steelblue", bins = 30) + 
      labs(title = paste("Histogram of ", col), x = col, y = "Total") + 
      theme_minimal()
    
    # print the plot
    print(plt)
    
    # create the output plot name
    plot_name <- paste(col, '_histogram.png', sep = '')
    
    if (!is.na(output_dir)) {
      # create output file path
      output_fpath <- file.path(output_dir, plot_name)
      # save plot
      ggsave(output_fpath)
    }

  }
  return(0)
}