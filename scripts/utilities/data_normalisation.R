#' Data Normalisation
#' 
#' @description Normalises all data in a dataframe to have 0 mean and standard deviation 1
#' 
#' @param dataset A dataframe with numeric columns to normalise
#' 
#' @return Returns the normalised numeric dataframe
#' 
data_normalisation <- function(dataset) {
  # range standardization [lower bound, upper_bound], e.g. [0, 1]
  # Convert data to have minimum 0 and maximum 1
  mean <- mean(dataset)
  standard.deviation <- sd(dataset)
  ((dataset - mean) / standard.deviation)
}