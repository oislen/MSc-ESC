# define a function to convert of the columns of a data frame to factor variables
column_to_factor <- function(dataset, col_names){
  # loop through the columns to be converted
  for (col in col_names){
    # convert the column to factor
    dataset[, col] <- as.factor(dataset[, col])
  }
  return(dataset)
}