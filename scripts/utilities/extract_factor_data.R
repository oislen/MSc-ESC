library("dplyr")

#' Extract Factor Data
#' 
#' @description Extracts all factor columns from a dataframe
#' 
#' @param dataset A dataframe with factor columns to extract
#' 
#' @return Returns the factor columns as a separate dataframe
#' 
extract_factor_data <- function(dataset) {
  # This function extracts the categorical attributes out of a data set and stores them in a separate data set
  # data set: the specified data set to extract the categorical attributes from
  
  # determine the factor columns in the data set
  fact_col <- names(which(sapply(X = dataset, FUN = function(x) is.factor(x) || is.character(x))))

  # extract out the numeric columns
  factor_data <- dataset %>% subset(select = fact_col)
  
  return(factor_data)
}
