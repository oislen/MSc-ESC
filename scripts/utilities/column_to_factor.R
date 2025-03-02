#' Column to Factor
#' 
#' @description Converts columns within a dataframe to factor data types
#' 
#' @param data A dataframe with columns to convert to factor data type
#' @param col_names A vector of column names to convert to factor data type
#' 
#' @return Returns a dataframe containing the converted factor data
#' 
column_to_factor <- function(
    dataset,
    col_names
    ){
  # loop through the columns to be converted
  for (col in col_names){
    # convert the column to factor
    dataset[, col] <- as.factor(dataset[, col])
  }
  return(dataset)
}