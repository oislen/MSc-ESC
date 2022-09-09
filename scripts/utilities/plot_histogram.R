# load relevant libraries
library(ggplot2)

# define function to plot and output histograms for given data.frame and vector of column names
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
    
    # create output file path
    output_fpath <- file.path(output_dir, plot_name)
    
    # save plot
    ggsave(output_fpath)

  }
  return(0)
}