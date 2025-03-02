source("utilities/data_normalisation.R")

#' Normalise Data
#' 
#' @description Normalises all data in a dataframe to have 0 mean and standard deviation 1
#' 
#' @param dataset A dataframe with numeric columns to normalise
#' 
#' @return Returns the normalised numeric dataframe
#' 
normalise_data <- function(dataset) {
  standardised_data <- dataset
  # data set: the specified data set to range standardized
  for (i in 1:ncol(dataset)) {
    if (is.numeric(dataset[,i])){
      standardised_data[,i] <- data_normalisation(dataset = dataset[,i])
    }
  }
  return(standardised_data)
}