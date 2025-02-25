# load relevant libraries
library(ggplot2)

# define function to plot and output bar charts for given data.frame and vector of column names
plot_bar_chart <- function(dataset, col_names, output_dir = 'report/plots/bar_charts'){
  # use a for loop to generate a bar plot for each categorical attributes
  for (col in all_factors){
    
    # create the ploy
    plt = ggplot(data = dataset, 
                 mapping = aes(x = as.factor(dataset[,col]), 
                               fill = as.factor(dataset[,col])
                 )
    ) + 
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