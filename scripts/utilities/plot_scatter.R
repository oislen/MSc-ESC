# define a function to plot a scatter point with overlaid correlation / linear model results
plot_scatter <- function(data, col_names, output_dir = 'report/plots/scatterplots'){
  
  # use a for loop to generate correlation scatter plots
  for (col in col_names){
    
    # create the x-axis limits
    lims = c("1","2","3","4","5","6","7","8","","10","","12")
    
    # create the correlation scatter plot
    plt = ggplot(data = data, 
                 mapping = aes(x = Points, y = data[,col]
                 )
    ) + 
      geom_point(shape = 16, colour = "steelblue") + 
      labs(title = paste("Scatterplot of Points vs ",col), 
           x = "Points", 
           y = col
      )  +  
      geom_smooth(method ='lm', 
                  linetype = "dashed", 
                  color="darkred", 
                  fill = "red",
                  formula = y ~ x
      ) +
      scale_x_discrete(limits = lims) + 
      theme_minimal()
    
    # print the plot
    print(plt)
    
    # create the output plot name
    plot_name <- paste(col, '_vs_Points_Scatterplot.png', sep = '')
    
    # create output file path
    output_fpath <- file.path(output_dir, plot_name)
    
    # save plot
    ggsave(output_fpath)
  }
  return(0)
}