library(dplyr)
# (1) Function to extract numeric data
extract_numeric_data <- function(dataset) {
  # This function extracts the numeric attributes out of a data set and stores them in a separate data set
  # data set: the specified data set to extract the numeric attributes from
  
  # determine the numeric columns in the data set
  num_cols <- names(which(sapply(X = dataset, FUN = function(x) is.numeric(x))))

  # extract out the numeric columns
  numeric_data <- dataset %>% subset(select = num_cols)
  
  return(numeric_data)
}