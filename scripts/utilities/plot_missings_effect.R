library("ggplot2")

#' Plot Missing Effects
#' 
#' @description Plots the effects of removing rows with na values before and after
#' 
#' @param col_names The column to plot the missing value effects for
#' @param orig_data The original data with NA values
#' @param comp_data The completed data without NA values
#' @param output_dir The output directory to write the missing effects plots as .png files, default is "./report/plots/missings/"
#' 
#' @return Returns 0 for successful execution
#' 
plot_missings_effect <- function(
    col_names,
    orig_data,
    comp_data,
    output_dir="./report/plots/missings/"
    ){
  
  # loop through the column names
  for (col in col_names){
    
    # create the plot
    plt1 <- ggplot(data = orig_data, mapping = aes(x = orig_data[, col], fill = ESCdata[, col])) + 
      geom_bar(stat = "count", width = 0.7, fill = "steelblue") + 
      labs(title = paste("All Cases - Bar Chart of ", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      guides(fill = FALSE)
    
    if (!is.na(output_dir)){
      # save the plot
      ggsave(paste(output_dir, col, "All_Cases_Bar_Chart.png"))
    }
    
    # print the plot
    print(plt1)
    
    # create the plot
    plt2 <- ggplot(data = comp_data, mapping = aes(x = completedata[, col], fill = comp_data[, col])) + 
      geom_bar(stat = "count", width = 0.7, fill = "steelblue") + 
      labs(title = paste("Complete Cases - Bar Chart of ", col), x = col, y = "Count") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 60, hjust = 1)) +
      guides(fill = FALSE)
    
    if (!is.na(output_dir)){
      # save the plot
      ggsave(paste(output_dir, col, "Complete_Cases_Bar_Chart.png"))
    }
    
    # print the plot
    print(plt2)
    
  }
  return(0)
}
