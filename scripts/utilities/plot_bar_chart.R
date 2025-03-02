library(ggplot2)

#' Plot Bar Chart
#' 
#' @description Plots a bar chart for a given set of columns in a dataset
#' 
#' @param dataset The dataframe with categorical features
#' @param col_names The categorical features to plot with a bar chart
#' @param output_dir The output directory to write the bar chart as a .png file, default is 'report/plots/bar_charts'
#' 
#' @return Returns 0 for successful execution
#' 
plot_bar_chart <- function(
    dataset,
    col_names,
    output_dir='report/plots/bar_charts'
    ){
  # use a for loop to generate a bar plot for each categorical attributes
  for (col in all_factors){
    
    # create the ploy
    plt = ggplot(data = dataset, mapping = aes(x = as.factor(dataset[,col]), fill = as.factor(dataset[,col]))) + 
      geom_bar(stat = "count", width = 0.7, fill = "steelblue") + 
      labs(title = paste("Bar Chart of ", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      guides(fill = FALSE)
    
    # print the plot
    print(plt)
    
    # create the output plot name
    plot_name <- paste(col, '_bar_chart.png', sep = '')
    
    if (!is.na(output_dir)) {
      # create output file path
      output_fpath <- file.path(output_dir, plot_name)
      # save plot
      ggsave(output_fpath)
    }
    
  }
  return(0)
}